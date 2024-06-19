# --------------------------------------------------------------- #
# Script 6 
# Format dataset for Tableau map
# --------------------------------------------------------------- #

# Read in project and country datasets from previous script
all_projects_tidied <- readRDS("Outputs/all_projects_tidied_Jan24update_2901.rds")
country_table_final <- readRDS("Outputs/country_table_final_Jan24update.rds")

#View(tableau_projects_tidied[tableau_projects_tidied$Fund=="DSIT - Newton Fund",])
# 1) Join countries to project data ----

tableau_projects <- all_projects_tidied %>% 
  left_join(country_table_final, by = c("id" = "project_id"))

# tableau_projects$start_date <- substr(tableau_projects$start_date, 1, 10)
# tableau_projects$end_date <- substr(tableau_projects$end_date, 1, 10)
# tableau_projects$period_start <- substr(tableau_projects$period_start, 1, 10)
# tableau_projects$period_end <- substr(tableau_projects$period_end, 1, 10)

# renaming lead_org_name ####
#unique(tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "IMPERIAL COLLEGE LONDON", "Imperial College London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ROYAL HOLLOWAY, UNIVERSITY OF LONDON", "Royal Holloway, University of London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "SHEFFIELD HALLAM UNIVERSITY", "Sheffield Hallam University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "LIVERPOOL JOHN MOORES UNIVERSITY", "Liverpool John Moores University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "LOUGHBOROUGH UNIVERSITY", "Loughborough University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ULSTER UNIVERSITY", "Ulster University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "NATURAL RESOURCES INSTITUTE, UNIVERSITY OF GREENWICH", "Natural Resources Institute, University of Greenwich", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF EDINBURGH", "University of Edinburgh", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "LONDON NORTH WEST UNIVERSITY HEALTHCARE NHS TRUST", "London North West University Healthcare NHS Trust", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "MANCHESTER METROPOLITAN UNIVERSITY", "Manchester Metropolitan University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "HARPER ADAMS UNIVERSITY", "Harper Adams University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "QUEEN'S UNIVERISTY, BELFAST", "Queen's University, Belfast", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF SOUTHAMPTON", "University of Southampton", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "LONDON SCHOOL OF HYGIENE AND TROPICAL MEDICINE", "London School of Hygiene and Tropical Medicine", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "SCOTLAND'S RURAL COLLEGE (SRUC)", "Scotland's Rural College (SRUC)", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF NOTTINGHAM", "University of Nottingham", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERISTY OF ESSEX", "University of Essex", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "BIRKBECK, UNIVERSITY OF LONDON", "Birkbeck, University of London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "LANCASTER UNIVERSITY", "Lancaster University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ASTON UNIVERSITY", "Aston University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "BANGOR UNIVERSITY", "Bangor University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "INSTITUTE OF DEVELOPMENT STUDIES", "Institute of Development Studies", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "LONDON SCHOOL OF ECONOMICS AND POLITICAL SCIENCE", "London School of Economics and Political Science", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "DURHAM UNIVERSITY", "Durham University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF LEEDS", "University of Leeds", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF EAST LONDON", "University of East London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "OXFORD BROOKES UNIVERSITY", "Oxford Brookes University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF GLASGOW", "University of Glasgow", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF OXFORD", "University of Oxford", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF SUSSEX", "University of Sussex", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF YORK", "University of York", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "QUEEN MARY UNIVERSITY OF LONDON", "Queen Mary University of London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "BRUNEL UNIVERSITY LONDON", "Brunel University London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "NOTTINGHAM TRENT UNIVERSITY", "Nottingham Trent University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY COLLEGE LONDON", "University College London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF BATH", "University of Bath", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "TEESSIDE UNIVERSITY", "Teesside University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF BRISTOL", "University of Bristol", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "KING'S COLLEGE LONDON", "King's College London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "COVENTRY UNIVERSITY", "Coventry University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF LIVERPOOL", "University of Liverpool", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "NORTHUMBRIA UNIVERSITY", "Northumbria University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF WARWICK", "University of Warwick", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF CAMBRIDGE", "University of Cambridge", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF KENT", "University of Kent", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF WEST OF SCOTLAND", "University of West of Scotland", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "SWANSEA UNIVERSITY", "Swansea University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF EXETER", "University of Exeter", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF EAST ANGLIA", "University of East Anglia", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "AGA KHAN UNIVERSITY", "Aga Khan University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF BIRMINGHAM", "University of Birmingham", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF SHEFFIELD", "University of Sheffield", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "CARDIFF UNIVERSITY", "Cardiff University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ANGLIA RUSKIN UNIVERSITY", "Anglia Ruskin University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF CHESTER", "University of Chester", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF PORTSMOUTH", "University of Portsmouth", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF HULL", "University of Hull", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "NEWCASTLE UNIVERSITY", "Newcastle University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "INSTITUTE FOR FISCAL STUDIES", "Institute for Fiscal Studies", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "SIGHTSAVERS", "Sightsavers", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF ESSEX", "University of Essex", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "BRITISH INSTITUTE AT ANKARA", "British Institute at Ankara", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "SCHOOL OF ADVANCED STUDY, UNIVERSITY OF LONDON", "School of Advanced Study, University of London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "GOLDSMITHS, UNIVERSITY OF LONDON", "Goldsmiths, University of London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "SOAS, UNIVERSITY OF LONDON", "SOAS, University of London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "INTERNATIONAL INSTITUTE FOR ENVIRONMENT AND DEVELOPMENT", "International Institute for Environment and Development", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "BRITISH INSTITUTE IN EASTERN AFRICA", "British Institute in Eastern Africa", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF SOUTH WALES", "University of South Wales", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "OVERSEAS DEVELOPMENT INSTITUTE", "Overseas Development Institute", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "HERIOT-WATT UNIVERSITY", "Heriot-Watt University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "SCHOOL OF ORIENTAL AND AFRICAN STUDIES", "School of Oriental and African Studies", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "LEEDS BECKETT UNIVERSITY", "Leeds Beckett University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "BOURNEMOUTH UNIVERSITY", "Bournemouth University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF WESTMINSTER", "University of Westminster", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF THE ARTS LONDON", "University of the Arts London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "DE MONTFORT UNIVERSITY", "De Montfort University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "MIDDLESEX UNIVERSITY", "Middlesex University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "EDGE HILL UNIVERSITY", "Edge Hill University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "AIRBUS", "Airbus", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "CENTRE FOR AGRICULTURE AND BIOSCIENCE INTERNATIONAL", "Centre for Agriculture and Bioscience International", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "SATELLITE APPLICATIONS CATAPULT", "Satellite Applications Catapult", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "CLYDESPACE", "Clydespace", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ECOMETRICA", "Ecometrica", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ENVIRONMENT SYSTEMS", "Environment Systems", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "EXACTEARTH EUROPE LTD", "Exactearth Europe Ltd", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "INMARSAT", "Inmarsat", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "REZATEC", "Rezatec", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "RHEA GROUP", "Rhea Group", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "SATELLITE OCEANOGRAPHIC CONSULTANTS", "Satellite Oceanographic Consultants", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ASTROSAT", "Astrosat", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "INSTITUTE FOR ENVIRONMENTAL ANALYTICS, UNIVERSITY OF READING", "Institute for Environmental Analytics, University of Reading", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "VIVID ECONOMICS", "Vivid Economics", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "VIVID ECONOMICS|REMOTE SENSING APPLICATIONS CONSULTANTS LIMITED", "Vivid Economics|Remote Sensing Applications Consultants Limited", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "AVANTI COMMUNICATIONS", "Avanti Communications", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ASSIMILA LTD", "Assimila Ltd", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UK SPACE AGENCY", "UK Space Agency", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "EARTH OBSERVATION", "Earth Observation", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "EARTH-I", "Earth-I", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "EOSPHERE LIMITED", "Eospere Limited", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "GEOSPATIAL", "Geospatial", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "GEOSAS", "Geosas", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "GLOBAL MAPAID", "Global Mapaid", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "NATIONAL OCEANOGRAPHY CENTRE", "National Oceanography Centre", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNITED NATIONS DEVELOPMENT PROGRAMME|HR WALLINGFORD", "United Nations Development Programme|HR Wallingford", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "BRITISH GEOLOGICAL SURVEY", "British Geological Survey", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ROTHAMSTED RESEARCH", "Rothamsted Research", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "AGRICOMPAS", "Agricompas", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "HR WALLINGFORD", "HR Wallingford", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "NLA INTERNATIONAL", "NLA International", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "PIXALYTICS LTD", "Pixalytics Ltd", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "THE OPEN UNIVERSITY", "The Open University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "REMOTE SENSING APPLICATIONS CONSULTANTS LIMITED", "Remote Sensing Applications Consultants Limited", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "OMANOS ANALYTICS", "Omanos Analytics", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF READING", "University of Reading", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "FAUNA AND FLORA INTERNATIONAL", "Fauna and Flora International", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "CARIBOU SPACE", "Caribou Space", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "BRITISH ACADEMY", "British Academy", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF STRATHCLYDE", "University of Strathclyde", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF SALFORD", "University of Salford", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF BEDFORDSHIRE", "University of Bedfordshire", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "CITY, UNIVERSITY OF LONDON", "City, University of London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "QUEEN'S UNIVERSITY BELFAST", "Queen's University Belfast", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF WOLVERHAMPTON", "University of Wolverhampton", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "CENTRE FOR ENVIRONMENT FISHERIES AND AQUACULTURE SCIENCE", "Centre for Environment Fisheries and Aquaculture Science", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF LEICESTER", "University of Leicester", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "THE UNIVERSITY OF MANCHESTER", "The University of Manchester", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "NATIONAL INSTITUTE OF AGRICULTURAL BOTANY", "National Institute of Agricultural Botany", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "EARLHAM INSTITUTE", "Earlham Institute", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ROYAL BOTANIC GARDENS KEW", "Royal Botanic Gardens Kew", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF HERTFORDSHIRE", "University of Hertfordshire", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "CRANFIELD UNIVERSITY", "Cranfield University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "JOHN INNES CENTRE", "John Innes Centre", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF THE WEST OF ENGLAND", "University of the West of England", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "EDINBURGH NAPIER UNIVERSITY", "Edinburgh Napier University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF ABERDEEN", "University of Aberdeen", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF SURREY", "University of Surrey", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "STAFFORDSHIRE UNIVERSITY", "Staffordshire University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "KINGSTON UNIVERSITY", "Kingston University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "THE BRITISH MUSEUM", "The British Museum", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF LINCOLN", "University of Lincoln", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "BIRMINGHAM CITY UNIVERSITY", "Birmingham City University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF HUDDERSFIELD", "University of Huddersfield", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "THE JAMES HUTTON INSTITUTE", "The James Hutton Institute", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF PLYMOUTH", "University of Plymouth", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF WORCESTER", "University of Worcester", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ABERYSTWYTH UNIVERSITY", "Aberystwyth University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF LONDON", "University of London", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ROYAL VETERINARY COLLEGE", "Royal Veterinary College", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF CENTRAL LANCASHIRE", "University of Central Lancashire", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF NORTHAMPTON", "University of Northampton", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF DERBY", "University of Derby", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF BRIGHTON", "University of Brighton", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF STIRLING", "University of Stirling", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF ST ANDREWS", "University of St Andrews", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "OTT CONSULTING LTD", "OTT Consulting Ltd", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "PUBLIC HEALTH ENGLAND", "Public Health England", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "UNIVERSITY OF GREENWICH", "University of Greenwich", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "KEELE UNIVERSITY", "Keele University", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ROYAL SOCIETY", "Royal Society", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ACADEMY OF MEDICAL SCIENCES", "Academy of Medical Sciences", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "BRITISH COUNCIL", "British Council", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "ROYAL ACADEMY OF ENGINEERING", "Royal Academy of Engineering", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "0", NA, tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "Vaxcyte (was SutroVax, renamed May 2020)", "Vaxcyte", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "Martin Maiden, University of Oxford", "University of Oxford", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "John Edmunds, LSHTM", "London School of Hygiene & Tropical Medicine", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "London Sch of Hygiene and Trop Medicine", "London School of Hygiene & Tropical Medicine", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "London School of Hygiene and Tropical Medicine", "London School of Hygiene & Tropical Medicine", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "University of Oxford - Jenner Institute", "University of Oxford, Jenner Institute", tableau_projects$lead_org_name)
tableau_projects$lead_org_name <- ifelse(tableau_projects$lead_org_name == "University of Oxford (Jenner Institute)", "University of Oxford, Jenner Institute", tableau_projects$lead_org_name)


tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "ROYAL ACADEMY OF ENGINEERING", "Royal Academy of Engineering", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "BRITISH ACADEMY", "British Academy", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "ACADEMY OF MEDICAL SCIENCES", "Academy of Medical Sciences", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "ROYAL SOCIETY", "Royal Society", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "UK SPACE AGENCY", "UK Space Agency", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "MRC", "Medical Research Council", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "Medical Research Council (MRC)", "Medical Research Council", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "EPSRC", "Engineering & Physical Sciences Research Council", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "ESPRC", "Engineering & Physical Sciences Research Council", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "BBSRC", "Biotechnology & Biological Sciences Research Council", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "Biotechnology and Biological Sciences Research Council (BBSRC)", "Biotechnology & Biological Sciences Research Council", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "Economic and Social Research Council (ESRC)", "Economic & Social Research Council", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "DEFRA", "Department for Environment, Food, and Rural Affairs", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "DHSC", "Department of Health and Social Care", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "Hivos}", "Hivos", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "IMC WORLDWIDE LTD", "IMC Worldwide Ltd", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "Natural Environment Research Council (NERC)", "Natural Environment Research Council", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "Uk Research And Innovation", "UK Research and Innovation", tableau_projects$extending_org)
tableau_projects$extending_org <- ifelse(tableau_projects$extending_org == "NIHR", "National Institute for Health and Care Research", tableau_projects$extending_org)


# 2) Remove unnecessary country "unknown" records --------

# Identify projects with no country info whatsoever
project_country_unknowns <- filter(tableau_projects, Country == "Unknown") %>%
  select(id, country_type) %>%
  unique() %>%
  group_by(id) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# Delete any other unknown records - these don't need displaying in Tableau
tableau_projects <- tableau_projects %>% 
  filter(id %in% project_country_unknowns$id | Country != "Unknown") 


# 3) Add funder programme names ------------------

# Add FCDO/DHSC programme names to dataset

    # Create vector of gov funder programme IATI IDs
    # (takes 10-15 mins to run)
    gov_funder_iati_ids <- tableau_projects %>% 
      select(Funder, iati_id) %>% 
      filter(str_detect(iati_id, "GB-1-|GB-GOV-1-|GB-GOV-10-")) %>% # filter FCDO and DHSC IDs only
        # remove any FCDO component numbers
        mutate(programme_iati_id = if_else(Funder == "Foreign, Commonwealth and Development Office" &
                                   substr(iati_id, nchar(iati_id)-3, nchar(iati_id)-3) == "-",
                                   substr(iati_id, 1, nchar(iati_id)-4), iati_id))

prog_name_extract <- function(o_code) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- extract_iati_activity_name(page, o_code)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

uk_activity_ids = unique(gov_funder_iati_ids$programme_iati_id)
uk_activity_ids <- URLencode(uk_activity_ids)

# batch_size = 15
# batches = c()
# current_batch = c()
# for(i in 1:length(uk_activity_ids)){
#   current_id = uk_activity_ids[i]
#   if(i %% batch_size == 0){
#     current_batch_str = paste0('("', paste(current_batch, collapse = '" OR "'), '")')
#     batches = c(batches, current_batch_str)
#     current_batch = c(current_id)
#   } else {
#     current_batch = c(current_batch, current_id)
#   }
# }

programme_extract <- lapply(uk_activity_ids, prog_name_extract)
programme_extract = rbindlist(programme_extract, fill=TRUE)

programme_extract <- programme_extract %>% 
  select(c(iati_identifier,title_narrative)) %>%
  rename(programme_iati_id = iati_identifier)

# Save to Rdata file
# saveRDS(programme_extract, file = "Outputs/last_programme_extract_Jan24update.rds")
programme_extract <- readRDS(file = "Outputs/last_programme_extract_Jan24update.rds")

gov_funder_iati_ids <- gov_funder_iati_ids %>%
  left_join(programme_extract, by = "programme_iati_id") %>%
  rename(funder_programme = title_narrative) %>%
  select(-Funder, -programme_iati_id) %>%
  unnest(funder_programme) %>%
  unique()

# %>% 
#         # Add programme names on
#         mutate(funder_programme = map(programme_iati_id, extract_iati_activity_name)) %>% 
#         mutate(funder_programme = unlist(funder_programme)) %>% 
#        select(-Funder, -programme_iati_id) %>% 
#        unique()

  
# Join funder programme name to main dataset
tableau_projects_tidied <- tableau_projects %>%
      left_join(gov_funder_iati_ids, by = "iati_id") %>% 
      mutate(funder_programme = if_else(extending_org == "Wellcome Trust", subject, funder_programme))

# View(tableau_projects_tidied[tableau_projects_tidied$Fund=="DHSC - Global Health Security - UK Vaccine Network",])
# 4) Apply manual exclusions/rules ----------------------------

# TEMPORARY ***
# Remove IDRC DHSC IATI data (this has been provided instead by spreadsheet)
# tableau_projects_tidied <- tableau_projects_tidied %>%
#   filter(!(Funder == "Department of Health and Social Care" &
#            extending_org == "International Development Research Centre")
#          )

# View(tableau_projects_tidied[tableau_projects_tidied$Fund=="DSIT - Newton Fund",])

tableau_projects_tidied$status <- ifelse(tableau_projects_tidied$iati_id %in% c("GB-1-111043"), "Closed", tableau_projects_tidied$status)
tableau_projects_tidied$iati_id <- ifelse(tableau_projects_tidied$iati_id %in% c("cGB-COH-RC000658-GB-GOV-1-301132"), "GB-COH-RC000658-GB-GOV-1-301132", tableau_projects_tidied$iati_id)

tableau_projects_tidied <- tableau_projects_tidied %>% relocate(iati_id, .before = id)
tableau_projects_tidied$iati_id <- ifelse(tableau_projects_tidied$iati_id == tableau_projects_tidied$id, NA, tableau_projects_tidied$iati_id)

tableau_projects_tidied$delivery_partner_id <- NA
tableau_projects_tidied <- tableau_projects_tidied %>% relocate(delivery_partner_id, .before = id)
tableau_projects_tidied$iati_id <- str_trim(tableau_projects_tidied$iati_id, "left")

tableau_projects_tidied$delivery_partner_id <- ifelse(!str_starts(tableau_projects_tidied$iati_id, "GB-GOV-|GB-1-"), tableau_projects_tidied$iati_id, NA)
tableau_projects_tidied$iati_id <- ifelse(!str_starts(tableau_projects_tidied$iati_id, "GB-GOV-|GB-1-"), NA, tableau_projects_tidied$iati_id)

tableau_projects_tidied$funder_proj_id <- NA
tableau_projects_tidied <- tableau_projects_tidied %>% relocate(funder_proj_id, .before = delivery_partner_id)

tableau_projects_tidied$funder_proj_id <- ifelse(grepl(".*-\\d{3}$", tableau_projects_tidied$iati_id), tableau_projects_tidied$iati_id, NA)
tableau_projects_tidied$iati_id <- ifelse(grepl(".*-\\d{3}$", tableau_projects_tidied$iati_id), sub("-\\d{3}$", "", tableau_projects_tidied$iati_id), tableau_projects_tidied$iati_id)

tableau_projects_tidied <- tableau_projects_tidied %>% rename(funder_prog_id = iati_id)

tableau_projects_tidied$funder_proj_id <- ifelse(str_starts(tableau_projects_tidied$id, "GB-GOV-1-|GB-1-") & grepl(".*-\\d{3}$", tableau_projects_tidied$id), tableau_projects_tidied$id, tableau_projects_tidied$funder_proj_id)

tableau_projects_tidied$funder_prog_id <- ifelse(tableau_projects_tidied$delivery_partner_id %in% c("GB-COH-RC000658-GB-GOV-1-301132"), "GB-GOV-1-301132", tableau_projects_tidied$funder_prog_id)
tableau_projects_tidied$funder_prog_id <- ifelse(tableau_projects_tidied$delivery_partner_id %in% c("GB-COH-RC000797-GB-GOV-1-300180"), "GB-GOV-1-300180", tableau_projects_tidied$funder_prog_id)
tableau_projects_tidied$funder_prog_id <- ifelse(tableau_projects_tidied$delivery_partner_id %in% c("GB-COH-05543952-GB-1-205222"), "GB-1-205222", tableau_projects_tidied$funder_prog_id)


dates_wrong <- 
tableau_projects_tidied %>% 
  filter(status=="Closed" & end_date > quarter_end_date) %>%
  select(id) %>%
  unique()

tableau_projects_tidied$status <- ifelse(tableau_projects_tidied$id %in% c(dates_wrong$id), "Active", tableau_projects_tidied$status)


tableau_projects_tidied[tableau_projects_tidied$id=="AH/S004025/1",]

# Restrict to active projects for Tableau
tableau_projects_tidied <- tableau_projects_tidied %>% 
  filter(status %in% c("Active", "Unknown") | end_date > "2018-12-31") %>% 
  unique()

gcrf_test <- tableau_projects_tidied[tableau_projects_tidied$Fund=="DSIT - Global Challenges Research Fund (GCRF)",]
unique(gcrf_test[gcrf_test$end_date > "2023-12-31",]$id)
newton_test <- tableau_projects_tidied[tableau_projects_tidied$Fund=="DSIT - Newton Fund",]
newton_test[newton_test$status=="Active",]

# 5) Write data --------------------------------


# tableau_projects_tidied <- tableau_projects_tidied %>% 
#   mutate(Fund = case_when(
#     Fund == "Global Challenges Research Fund (GCRF)" ~ "DSIT - Global Challenges Research Fund (GCRF)",
#     Fund == "Newton Fund" ~ "DSIT - Newton Fund",
#     Fund == "Chevening Scholarships" ~ "FCDO - Chevening Scholarships",
#     Fund == "Global Health Research - Partnerships" ~ "DHSC - Global Health Research - Partnerships",
#     Fund == "Global Health Research - Programmes" ~ "DHSC - Global Health Research - Programmes",
#     Fund == "Global Health Security - GAMRIF" ~ "DHSC - Global Health Security - GAMRIF",
#     Fund == "Global Health Security - UK Vaccine Network" ~ "DHSC - Global Health Security - UK Vaccine Network",
#     Fund == "International Climate Finance (ICF)" ~ "DSIT - International Climate Finance (ICF)",        
#     TRUE ~ Fund
#   ))

# remove unallocated budget

tableau_projects_tidied <- tableau_projects_tidied %>% filter(title != "Unallocated budget")


# Write to RDS 
saveRDS(tableau_projects_tidied, "Outputs/tableau_projects_tidied_Jan24update_2901.rds")
# tableau_projects_tidied <- readRDS("Outputs/tableau_projects_tidied_Jan24update.rds")
write.xlsx(tableau_projects_tidied, file = "Outputs/tableau_projects_tidied_Jan24update_2901.xlsx")


# comparing sector codes in AMP with what came from IATI



# creating a version to be downloaded from the modari map without splitting out by country

tableau_projects_tidied_dwnld <- tableau_projects_tidied %>% 
  select(!country_type) %>%
  unique() %>%
  group_by(id) %>%
  summarise(Country = paste(coalesce(Country, ""), collapse = ", "))

tableau_projects_tidied_tosave <- tableau_projects_tidied %>%
  select(-country_type, -Country) %>%
  unique() %>%
  left_join(tableau_projects_tidied_dwnld, by = "id")


write.xlsx(tableau_projects_tidied_tosave, file = "Outputs/tableau_projects_tidied_Jan24update_dwnld_2901.xlsx")


# Write data to EC google drive 
# Authorise googlesheets4 to view and manage sheets on EC Drive
# (using saved authentication token in folder)

# ODA_RI_url <- "https://docs.google.com/spreadsheets/d/1ByVBWb3LNSoqAUzKlddd537DleQ-y9MINwY_SuuZEbY/edit#gid=2024786204"
# results <- as_sheets_id(ODA_RI_url)
# 
# results_sheet <- sheet_write(tableau_projects_tidied,
#                              ss = results,
#                              sheet = "ODA_RI_projects")


fcdo_summary <- tableau_projects_tidied %>% 
  filter(Funder == "Foreign, Commonwealth and Development Office") %>%
  filter(!is.na(funder_programme)) %>%
  group_by(funder_prog_id, funder_programme, status) %>%
  summarise(length_ext_org = length(unique(extending_org)))

fcdo_summary_active <- fcdo_summary %>% filter(status == "Active")
fcdo_summary_active[fcdo_summary_active$length_ext_org >1,]
