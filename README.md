# Mapping ODA-funded research and innovation (MODARI) - project data

## Background
This work collates information on research and innovation activities funded by UK Official Development Assistance (ODA) across all UK government funders and external partners. R scripts are used to extract and process project data, either shared in spreadsheet format or extracted from funders' public data repositories. Data on ODA research spend and active projects is then visualised in public Power BI and Tableau dashboards. These are designed to be accessible to a wide range of stakeholders and can be searched by funder, country and external partner.

Data updates will occur each quarter to pick up new ODA projects and changes to existing ones. These scripts are designed to be a reproducible analytical pipeline to update the public dashboards after each data update.

## Repo contents

### R Scripts
The following scripts are designed to run sequentially.

**Dashboard 2 (script 0) - IATI linked partner activities.R** - set up script that loads required R packages, reads in reference/input data and defines functions used in later scripts

**Dashboard 1 - IATI_API_research_extract.R** - extracts all ODA research and innovation activities from UK government funders' published IATI transparency data. Relevant activities are either identified by fund (e.g. BEIS GCRF/Newton) or a relevant research OECD sector code.

**Dashboard 2 (script 1) - IATI linked partner activities.R** - extracts all IATI activities published by external partners that are linked to a UK government funder ODA programme via an activity ID in the partner's incoming funds. This is exclusively FCDO partners currently

**Dashboard 2 (script 2) - IATI (non-linked) partner activities.R** - extracts other IATI activities published by external partners, either manually identified and listed in the Inputs spreadsheet *IATI partner activities.xlsx* or identified from specific research partners like CGIAR, CABI and Bill & Melinda Gates Foundation as being funded by UK ODA

**Dashboard 2 (script 3) - ODA RI awards collation.R** - master script that extracts ODA award data from public repositories (UKRI Gateway to Research, 
NIHR Open Data), reads in other funder data (Wellcome, IATI data from previous scripts, BEIS RODA extracts), formats for consistency and collates.

**Dashboard 2 (script 4) - finalisation and country extraction.R** - separates and cleans the country field for feeding into a Tableau map. Applies manual exclusions to the data and saves to a Google Sheet.

### Inputs

* GRID tables: references tables in Excel from the online GRID database (https://www.grid.ac/). These are used to look up the country location of research institutions
* IATI returns: project data in spreadsheet format - i.e. that is not yet linked or correctly published to the IATI Registry. It is read in manually to the ODA project database
* Country lookup - Tableau and DAC Income Group.xlsx - reference table to clean country data for Tableau and match to the World Bank income group
* BEIS RODA extracts (Excel): GCRF and Newton project data provided directly by BEIS, extracted from their Reporting ODA (RODA) platform
* IATI partner activities: IATI activity IDs of external partner data that should be included in the ODA project database but are not correctly linked in IATI so that they get pulled in in an automated way
* Wellcome grants.xlsx: ODA grant data provided quarterly by the Wellcome Trust, identified via the UK government partner
* UKRI non GCRF-Newton projects: UKRI Gateway to Research IDs for ODA projects co-funded by DHSC or FCDO (non GCRF/Newton). These fall under UKRI's "other ODA" funding and are not currently identifiable on the Gateway to Research website (https://gtr.ukri.org/). These have been provided directly by UKRI.

## Data visualisations

Output from the two scripts above feed two visualisations of ODA funded research & innovation:

*Dashboard 1: (Power BI)
https://app.powerbi.com/view?r=eyJrIjoiOTg1ODdlYjctNjEwOC00MWJmLWFiOGQtZjQ1NjU0MzhmNjI3IiwidCI6IjNiN2E2NzVhLTFmYzgtNDk4My1hMTAwLWNjNTJiNzY0NzczNyIsImMiOjh9

*Dashboard 2: Funded ODA R&I awards by country: 
https://public.tableau.com/views/ODARIprojectsv3/MainPage?:language=en-US&:display_count=n&:origin=viz_share_link

## Exploration and Analysis

Various scripts using the collated project data for analyses - e.g. *"Country active project reports.R"* outputs a formatted Excel list of active projects by country 


