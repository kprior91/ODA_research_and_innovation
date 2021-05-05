# Mapping ODA-funded research and innovation

## Background
This work aims to collate information on UK Official Development Assistance (ODA) research and innovation activities across all government funders and externals partners in a single place. Stakeholders should be able to view funded R&I activities by country, organisation and funder.

R scripts are used to extract and process data, either shared directly by funders or extracted from funders' public data repositories. Data on funded awards is then visualised in public Tableau and Power BI dashboards.

## Repo contents
The following scripts are designed to run sequentially.

**Dashboard 1 - IATI_API_research_extract.R** - extracts all ODA research and innovation activities from government funders' published IATI data. Relevant activities are either identified by fund (e.g. BEIS GCRF/Newton) or a relevant research OECD sector code.

**Dashboard 2 (script 1) - IATI linked partner activities.R** - extracts all IATI activities published by external partners that are linked to a UK government funder ODA programme via an activity ID in the partner's incoming funds

**Dashboard 2 (script 2) - IATI (non-linked) partner activities.R** - extracts other IATI activities published by external partners, either manually identified and listed in the Inputs spreadsheet *IATI partner activities.xlsx* or identified from specific research partners as being funded by UK ODA

**Dashboard 2 (script 3) - ODA RI awards collation.R** - master script that extracts ODA award data from public repositories (UKRI Gateway to Research, 
NIHR Open Data), reads in other funder data (Wellcome, IATI from previous scripts), collates and formats it for output to a Google Sheet.

## Data visualisations

Output from the two scripts above feed two visualisations of ODA funded research & innovation:

*Dashboard 1: (Power BI)
https://app.powerbi.com/view?r=eyJrIjoiOTg1ODdlYjctNjEwOC00MWJmLWFiOGQtZjQ1NjU0MzhmNjI3IiwidCI6IjNiN2E2NzVhLTFmYzgtNDk4My1hMTAwLWNjNTJiNzY0NzczNyIsImMiOjh9

*Dashboard 2: Funded ODA R&I awards by country: 
https://eur02.safelinks.protection.outlook.com/?url=https%3A%2F%2Fpublic.tableau.com%2Fshared%2FW355QTQND%3F%3Adisplay_count%3Dy%26%3Aorigin%3Dviz_share_link&data=04%7C01%7Ce-clegg%40dfid.gov.uk%7C0cece64d2ef044f48d8008d8d33f6a4d%7Ccdf709af1a184c74bd936d14a64d73b3%7C0%7C0%7C637491616620254639%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C1000&sdata=7Fm810nUXEUF8ZVOPZYzeQqv0DiDnYWw47UOO%2FsgVFk%3D&reserved=0



