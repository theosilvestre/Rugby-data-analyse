# Rugby-data
Obtaining and analysing TOP14 data:
- scrape.R construct TOP14 2025/2026 season dataset.
- test_dataset.R gives certain satistics of the TOP14 that can be compared to online data on the LNR website (number of cards, number of tries and points per team).
- Cards data is not consistent, it seems that at least one red and one orange card are missing and there is one yellow card that is not counted on the website of the LNR (to date round 21 season 2025/2026).

Why data are scraped from rugbyrama.fr ?
- Because data are from an official provider: iDalgo
- They take into account stoppage time. It is common with other providers that a try scored at 40'+2 minutes is labeled 42' which of course interfer with tries from the second half.
- However, there are still some minor issues (corrected at the end of scrape.R).
