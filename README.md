# Mapping ODA-funded research and innovation (MODARI) - project data

## Background
This work collates information on research and innovation activities funded by UK Official Development Assistance (ODA) across UK government funders and external partners. The R scripts extract and process project data, either shared in spreadsheet format or extracted from public data repositories. 

Outputs include an interactive Tableau map of active projects on the UKCDR website (https://www.ukcdr.org.uk/what-we-do/our-work/data-on-uk-funded-research-and-innovation/) and formatted Excel lists of projects by country. 

The data processing scripts should be rerun quarterly to pick up new ODA projects and changes to existing ones. Some funders (BEIS and the Wellcome Trust) are currently sharing cumulative data extracts by spreadsheet for these quarterly updates.

## Repo contents

### R Scripts
The following scripts are designed to run sequentially.

**Script 0 - set up.R** - set up script that loads required R packages, reads in reference/input data and defines functions used in later scripts.

**Script 1 - UK gov funder IATI research extract.R** - extracts all ODA research and innovation activities from UK government funders' published IATI transparency data. Relevant activities are either identified by fund (e.g. BEIS GCRF/Newton) or a relevant research OECD sector code.

**Script 2 - IATI linked partner activities.R** - extracts all IATI activities published by external partners that are linked to a UK government funder ODA programme via an activity ID in the partner's transactions. This applies predominantly to FCDO partners currently, though should widen as more partners of other government departments begin to publish to IATI.

**Script 3 - IATI (non-linked) partner activities.R** - extracts other IATI activities published by external partners, either manually listed in the Inputs spreadsheet *IATI partner activities.xlsx* or  from taken from specific research partners like CGIAR, CABI and the Bill & Melinda Gates Foundation.

**Script 4 - ODA RI projects collation.R** - master script that extracts ODA award data from public repositories (IATI Registry, UKRI Gateway to Research, NIHR Open Data), and collates with other funder data supplied by spreadsheet (Wellcome ODA grants, BEIS GCRF and Newton Fund).

**Script 5 - FCDO SQL database save.R** - separates project data into 4 relational SQL tables to save to a database on FCDO's AMP server (access by request, available to FCDO internal staff)

**Script 6 - Output 1 (active projects for Tableau).R** - separates and cleans the country field for displaying data in the UKCDR website Tableau map. Applies manual exclusions to the data and saves to a Google Sheet.

**Script 7 - Output 2 (country active project lists).R** - outputs a formatted Excel list of active projects for specified countries

**Script 8 - Output 3 (top organisations by country).R** - outputs summary tables of most common institutions by country, based on the number of projects they are involved in

**Script 9 - automated testing.R** - automated tests of scripts and outputs for quality assurance


### Inputs

* *GRID tables:* references tables in Excel from the online GRID database (https://www.grid.ac/). These are used to look up the country location of research institutions
* *IATI returns:* project data in spreadsheet format - i.e. that is not yet linked or correctly published to the IATI Registry. It is read in manually to the ODA project database
* *Country lookup - Tableau and DAC Income Group.xlsx* - reference table to clean country data for Tableau and match to the World Bank income group
* *BEIS RODA extracts (Excel):* GCRF and Newton project data provided directly by BEIS, extracted from their Reporting ODA (RODA) platform. These should be overwritten by a new cumulative extract each quarter (supplied by BEIS to UKCDR 1.5 months after quarter end)
* *IATI partner activities.xlsx:* IATI activity IDs of external partner data that should be included in the ODA project database but are not correctly linked in IATI. This means they do not get pulled in in an automated way
* *Wellcome grants.xlsx:* ODA grant data provided quarterly by the Wellcome Trust, identified via the UK government partner
* *UKRI non GCRF-Newton projects.xlsx:* UKRI Gateway to Research IDs for ODA projects co-funded by DHSC or FCDO (non GCRF/Newton). These fall under UKRI's "other ODA" funding and are not currently identifiable on the Gateway to Research website (https://gtr.ukri.org/). These have been provided directly by UKRI.


## Outputs

**Script 6 - Output 1 (active projects for Tableau).R** saves a formatted data extract to a Google Sheet to feed the Tableau map below of active ODA funded research & innovation projects:

* Tableau map - Funded ODA R&I projects by country: 
https://www.ukcdr.org.uk/what-we-do/our-work/data-on-uk-funded-research-and-innovation/

This map is designed for a wide range of stakeholders to search the project data by funder, country and external partner. More detail and a user guide can be found on the UKCDR webpage.


**Script 7 - Output 2 (country active project lists).R** outputs a formatted Excel list of active projects for countries the user specifies in the script. These have been passed to country-based development staff and Heads of Mission to inform them of activity relevant to their country.


**Script 8 - Output 3 (top organisations by country).R** - outputs summary tables of most common institutions by country, based on the number of projects they are involved in.

This could do with further development to improve coverage of country locations for different organisations. As it stands, the script returns a list of the known institutions for a given country.


## Exploration and Analysis

Various scripts using the collated project data for analyses


