# Mapping ODA-funded research and innovation projects

## Background
This work collates information on research and innovation activities funded by UK Official Development Assistance (ODA) across all UK government funders and external partners. R scripts are used to extract and process project data, either shared in spreadsheet format or extracted from funders' public data repositories. Data on ODA research spend and active projects is then visualised in public Power BI and Tableau dashboards. These are designed to be accessible to a wide range of stakeholders and can be searched by funder, country and external partner.

Data updates will occur each quarter to pick up new ODA projects and changes to existing ones. These scripts are designed to be a reproducible analytical pipeline to update the public dashboards after each data update.

## Repo contents
The following scripts are designed to run sequentially.

**Dashboard 2 (script 0) - IATI linked partner activities.R** - set up script that loads required R packages, reads in reference/input data and defines functions used in later scripts

**Dashboard 1 - IATI_API_research_extract.R** - extracts all ODA research and innovation activities from UK government funders' published IATI transparency data. Relevant activities are either identified by fund (e.g. BEIS GCRF/Newton) or a relevant research OECD sector code.

**Dashboard 2 (script 1) - IATI linked partner activities.R** - extracts all IATI activities published by external partners that are linked to a UK government funder ODA programme via an activity ID in the partner's incoming funds. This is exclusively FCDO partners currently

**Dashboard 2 (script 2) - IATI (non-linked) partner activities.R** - extracts other IATI activities published by external partners, either manually identified and listed in the Inputs spreadsheet *IATI partner activities.xlsx* or identified from specific research partners like CGIAR, CABI and Bill & Melinda Gates Foundation as being funded by UK ODA

**Dashboard 2 (script 3) - ODA RI awards collation.R** - master script that extracts ODA award data from public repositories (UKRI Gateway to Research, 
NIHR Open Data), reads in other funder data (Wellcome, IATI data from previous scripts, BEIS RODA extracts), formats for consistency and collates.

**Dashboard 2 (script 4) - finalisation and country extraction.R** - separates and cleans the country field for feeding into a Tableau map. Applies manual exclusions to the data and saves to a Google Sheet.

## Data visualisations

Output from the two scripts above feed two visualisations of ODA funded research & innovation:

*Dashboard 1: (Power BI)
https://app.powerbi.com/view?r=eyJrIjoiOTg1ODdlYjctNjEwOC00MWJmLWFiOGQtZjQ1NjU0MzhmNjI3IiwidCI6IjNiN2E2NzVhLTFmYzgtNDk4My1hMTAwLWNjNTJiNzY0NzczNyIsImMiOjh9

*Dashboard 2: Funded ODA R&I awards by country: 
https://eur02.safelinks.protection.outlook.com/?url=https%3A%2F%2Fpublic.tableau.com%2Fshared%2FW355QTQND%3F%3Adisplay_count%3Dy%26%3Aorigin%3Dviz_share_link&data=04%7C01%7Ce-clegg%40dfid.gov.uk%7C0cece64d2ef044f48d8008d8d33f6a4d%7Ccdf709af1a184c74bd936d14a64d73b3%7C0%7C0%7C637491616620254639%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C1000&sdata=7Fm810nUXEUF8ZVOPZYzeQqv0DiDnYWw47UOO%2FsgVFk%3D&reserved=0

## Exploration and Analysis

Various scripts using the collated project data for analyses - e.g. *"Country active project reports.R"* outputs a formatted Excel list of active projects by country 


