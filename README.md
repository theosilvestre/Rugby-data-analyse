# Rugby-data
Obtaining and analysing TOP14 data using JS for scraping (scrapeJS/main.mjs). Targeted websites are rugbyrama.fr, rugbypass.com and top14.lnr.fr. It also usable for other championships on those websites. The folder old.scrapeR provide an out-of-date R version for scraping.

# Observations

The LNR data seems to have a 2-minute delay for the match actions; rugbyrama and rugbypass are more accurate. However rugbyrama seems to include many errors. The times for player changes are good, but there are a lot of mistakes for rugbyrama. Rugbyrama is the only one to record penalties and missed drops. Rugbypass has a lot of missing matches.

# Data providers

- Rugbyrama seems to go through IDalgo, which is a data aggregator; there isn’t any more information about the data producer behind IDalgo.
Source: The rugbyrama website intensively uses classes such as "li_idalgo_content_match_action_part","li_idalgo_content_match_action_part_1", more than 3,800 references to idalgo on the page https://www.rugbyrama.fr/resultats/rugby/top-14/phase-reguliere/rencontre/55924/stade-francais-montauban for example.

- Rugbypass seems to go through Opta.
Source: "{"type":"opta","season":2025,"years":{"2024":2024","2025":2025},"label":"2024\/2025","completed":1,"standings":1,"started":1,"months":0,"games":245}" over 250 citations at https://www.rugbypass.com/pro-d2/fixtures-results/

- LNR does not provide any information on its website about the data provider. The data on the site does not seem to be directly linked to a supplier. However, the LNR states to initiate "a market consultation in order to select the operator who will manage the sports statistics of its championships from the 2027/2028 season." source: https://www.lnr.fr/actualite/consultation-pour-la-selection-de-loperateur-de-statistiques-sportives-de-la-lnr
