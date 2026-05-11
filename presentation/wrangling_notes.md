# Data wrangling notes — for Q/A prep

A plain-English walkthrough of how the dataset was built, in case a judge asks
"so how did you actually get this data and clean it up?" Read this once before
the presentation and you'll have an answer for every step.

---

## 1. Where the data came from

The Y Combinator company directory at `ycombinator.com/companies` is a
JavaScript-rendered page — if you `curl` it you get a near-empty shell. The
real data is fetched client-side from a **public Algolia search index** (Algolia
is a hosted search-as-a-service product). I confirmed this by opening the page
in Chrome DevTools → Network tab → filtering for `algolia.net`, and watched the
browser POST queries with the search box keystrokes.

So instead of scraping the rendered HTML (slow, fragile, would need a headless
browser), I hit the same Algolia endpoint the website itself uses:

- **App ID:** `45BWZJ1SGC`
- **Index:** `YCCompany_production`
- **Endpoint:** `https://45BWZJ1SGC-dsn.algolia.net/1/indexes/YCCompany_production/query`
- **Auth:** the public search-only API key embedded in the page HTML — this
  same key is what every visitor's browser uses, so I'm not bypassing any auth.

In R this is just `httr2::request()` with the Algolia headers and a JSON body.
Code lives in `R/01_scrape_yc.R`.

**Politeness:** 1 request/second (`req_throttle(rate = 1)`), an identifying
User-Agent (`Lawrence-University-Datathon-Project (a.mamyrbay@gmail.com)`),
and `req_retry(max_tries = 3)` for transient failures. Total scrape ≈ 50
seconds for 5,884 companies.

## 2. The pagination problem and the fix

This is the most interesting wrangling story — worth knowing for Q/A.

Algolia caps public search results at `paginationLimitedTo=1000`. A naive
`hitsPerPage=1000&page=0..N` loop returns at most 1,000 hits no matter how many
you ask for. My first scrape returned exactly 1000 — obviously wrong, the
directory has thousands.

**The fix: facet by batch.** Algolia's facet API lets you ask "for each value
of field X, how many results match?" without paginating. So I:

1. Make one zero-results query asking for the `batch` facet:
   `hitsPerPage=0&facets=["batch"]&maxValuesPerFacet=500`. Algolia replies with
   the count of companies in every batch (49 batches in the data, max 399
   companies in any single batch — well under the 1000 cap).
2. For each batch, send a filtered query:
   `hitsPerPage=1000&facetFilters=["batch:Winter 2022"]`. Each one returns the
   full batch, and 49 small queries × ≤399 hits each = the entire 5,884.
3. Concatenate, dedupe by `id` (just in case), save to JSON.

Result: `data/raw/yc_raw_20260507.json` — 5,884 unique company records,
matches Algolia's reported total exactly.

## 3. What a raw record looks like

One company, exactly as it comes out of Algolia (excerpted; some fields
truncated for readability):

```json
{
  "id": 26331,
  "name": "BBy",
  "slug": "bby",
  "former_names": ["BBy, Inc"],
  "website": "http://www.bbymilk.com",
  "all_locations": "New York, NY, USA",
  "long_description": "BBy's condensing device turns milk into a fine powder...",
  "one_liner": "BBy powders breast milk that's immunologically active & lasts 6 months",
  "team_size": 70,
  "industry": "Healthcare",
  "subindustry": "Healthcare -> Medical Devices",
  "launched_at": 1643810815,
  "tags": ["Health Tech", "Medical Devices", "Healthcare"],
  "top_company": false,
  "isHiring": false,
  "nonprofit": false,
  "batch": "Winter 2022",
  "status": "Active",
  "industries": ["Healthcare"],
  "regions": ["America / Canada"],
  "stage": "Early"
}
```

Things to notice that drove the wrangling:

- `batch` is a free-form string like `"Winter 2022"`, not a year.
- `team_size` is the **current** size, not a bucket — also can be `null`.
- `launched_at` is a **Unix epoch timestamp** (seconds since 1970), not a date.
- `status` is one of four strings: `Active`, `Acquired`, `Public`, `Inactive`.
- `tags`, `industries`, `regions` are JSON **arrays** — one company can be
  tagged with multiple industries.
- `all_locations` can be a single string or a **semicolon-joined list** for
  companies with multiple HQs, e.g. `"San Francisco, CA, USA; Remote"`.
- Each location is itself a **comma-separated** "city, state, country" — but
  not every entry has all three (international ones are often "city, country").

## 4. Wrangling steps (what `R/02_clean_wrangle.R` does)

Each step exists to fix a specific problem in the raw shape. I'll list them in
order with the *why*.

### 4.1 Flatten nested arrays

`tags`, `industries`, `regions` come in as JSON arrays. For a tidy CSV I
collapse each one into a single semicolon-joined string per company (e.g.
`"Health Tech;Medical Devices;Healthcare"`). I keep the joined version for the
wide table, and **separately** explode `industries` into a long table for
tag-level analysis (see step 4.10).

### 4.2 Parse `batch` → year + season

`parse_batch()` in `R/utils.R` uses a regex to split `"Winter 2022"` into
`batch_season = "Winter"` and `batch_year = 2022L`. The regex also handles YC's
old "International" batches (`IK12` etc.) by accepting an `IK\d*` prefix.
`batch_year` is the foundation of every time-series chart in the deck.

### 4.3 Bucket `team_size` into an ordered factor

A scatter plot of "raw team_size vs success rate" would be unreadable —
companies range from 1 person to 10,000+. So `bucket_team_size()` collapses
the numeric into six ordered bins:

```
1-5  →  6-10  →  11-25  →  26-100  →  101-500  →  500+
```

NA or 0 stays NA (no team data). The factor is `ordered = TRUE` so ggplot
plots them left-to-right correctly without me having to specify the order
every time.

### 4.4 Derive `outcome` from `status`

The headline metric of the project. `derive_outcome()` collapses YC's four
statuses into three buckets:

| YC status | Project outcome |
|-|-|
| `Acquired`, `Public` | **Successful** |
| `Active` | **Surviving** |
| `Inactive` | **Failed** |

Why three not four? Because for "what makes a winner?" the difference between
Acquired and Public doesn't matter — both are exits. Combining them gives
larger sample sizes per industry/region/team-bucket cell, tighter Wilson CIs.

### 4.5 Split `all_locations` into city/region/country + is_remote

`split_location()` does several things in sequence to handle YC's messy
location strings:

1. Take the **first segment** before any `;` (the primary HQ; ignore secondary
   ones for analysis).
2. If the primary segment is literally `"Remote"`, set city/region/country to
   NA (Remote is a status, not a place).
3. If the original string contains `"Remote"` anywhere, set `is_remote = TRUE`
   regardless.
4. Split the primary segment on commas. If 3 parts → "city, region, country"
   (USA-style). If 2 parts → "city, country" (international). The country is
   always the **last** comma-separated piece.

This is the kind of code where a lot of edge cases live. Test it on a few
known examples before trusting it.

### 4.6 Flag SF Bay Area

`flag_sf_bay()` is a regex that matches city names in the Bay Area
(`san francisco|palo alto|mountain view|menlo park|...`). Used so the deck
can call out "in vs outside the Bay" without having to do real geocoding.

### 4.7 Flag US

`is_us()` accepts country values `"USA"`, `"US"`, `"United States"`, or
`"United States of America"` — YC isn't consistent. This becomes the `in_us`
boolean used in geography analysis.

### 4.8 Compute company_age and era

- `company_age = current_year - batch_year`. Used to filter "mature" batches
  for outcome analysis (companies need ~5 years to be plausibly Acquired or
  Public; recent batches haven't had time, which would bias the rate
  downward).
- `era = batch_era(batch_year)`: a two-level factor `2005-2014` vs
  `2015-2024`. This is the basis of Q3, the era-shift comparison.

### 4.9 Convert `launched_at` to a Date

The raw value is Unix seconds since 1970. `as.POSIXct(..., origin = "1970-01-01")`
converts it to a real datetime; `as.Date()` truncates to a calendar date for
plotting.

### 4.10 Build the "long" industries table

Final transformation: take the wide table (one row per company, industries
joined by `;`), call `separate_longer_delim(industries, delim = ";")`, and
write `yc_industries_long.csv` (10,536 rows — most companies have 1–2
industry tags, hence ~1.8× expansion from 5,884). This is the right shape for
"how often does Fintech co-occur with B2B?"-type questions, even though the
deck mostly uses the wide primary-industry view.

## 5. The two output tables

| File | Shape | One row = | Used for |
|-|-|-|-|
| `data/processed/yc_clean.csv` | 5,884 × 31 | one company | every Q1, Q2, Q3 chart in the deck |
| `data/processed/yc_industries_long.csv` | 10,536 × 12 | one (company, industry tag) | tag-level analysis (not used in headline charts) |

The deck filters `yc_clean.csv` to **5,880 across 46 batches** by dropping
Summer 2026 and Fall 2026 — those companies have just been admitted, the
batch hasn't started yet, and including them would inflate batch counts
relative to public sources.

## 6. Sanity checks I ran

- `nbHits` from Algolia (5884) == count of unique IDs in the JSON (5884). No
  silent truncation.
- No batch hit the 1000-cap warning during scraping.
- All `status` values are exactly one of {Active, Acquired, Public, Inactive}
  after `str_to_title()` — no typos or unexpected values.
- `parse_batch()` produces non-NA year for every row.
- `team_size` distribution is sensible (median ≈ 4, max in the thousands for
  companies like Stripe, Coinbase).

## 7. Caveats I'm ready to acknowledge in Q/A

These are also on the limitations slide of the deck.

- **Survivorship bias.** YC may de-emphasize Inactive companies in their
  directory (some shut-down companies disappear entirely). True failure rate
  is likely higher than the 17.6% I observe.
- **Right-censoring.** Companies in recent batches haven't had time to be
  Acquired or Public. All "exit hit rate" figures restrict to mature batches
  (`batch_year <= 2021`, ≈5 years to exit).
- **Self-reported labels.** Status, industry, team size are YC's own
  classifications, not audited.
- **Multi-HQ truncation.** I keep only the primary location. The `is_remote`
  flag captures companies that are remote-first or remote-secondary, but I
  don't analyze the secondary HQs separately.
- **Perfect separation in 500+ team bucket.** Among mature batches, every
  500+ company is Successful — a logistic regression with that bucket diverges
  numerically. Excluded from the logit; called out on the methodology slide.

## 8. One-sentence answers for likely questions

- "Where's the data from?" — Y Combinator's public company directory, scraped
  fresh on 2026-05-07 via the same Algolia search API the website uses.
- "How did you get more than 1000 results?" — Algolia caps pagination at
  1000, so I faceted by batch (49 batches, all under 1000 each) and made one
  filtered query per batch.
- "What's success?" — A company is Successful if YC lists it as Acquired or
  Public, Surviving if Active, Failed if Inactive. I combine Acquired + Public
  because the difference doesn't matter for "what predicts an exit?"
- "How do you handle recent batches?" — Anything younger than 5 years is
  excluded from outcome analysis (it hasn't had time to exit). The portfolio-
  evolution charts include all batches because they're about composition, not
  outcomes.
- "Why are you confident in the numbers?" — I report Wilson confidence
  intervals on every rate, exclude small cells, and call out survivorship and
  censoring explicitly. The logistic regression results corroborate the
  univariate findings.
