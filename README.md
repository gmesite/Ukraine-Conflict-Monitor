# Ukraine-Conflict-Monitor
A Shiny dashboard designed for the exporatory analysis of data from ACLED. The data is an event-based dataset, in spreadsheet form, where each row represents an event. The columns have spatial and temporal information, as well as event type, actors involved, etc. To access their data, visit the “Armed Conflict Location & Event Data Project (ACLED) website at www.acleddata.com.”. To see the dashboard in action see  . Unforunately the dashboard is not available for viewing online but will be available in September. 
https://www.youtube.com/watch?v=rkb_C4Q0X20

I have not included the ACLED data in github, as it is against ACLED's terms of service. However, spatial data for Ukraine's subnational administrative boundaries can be downloaded 
for free at https://data.humdata.org/dataset/cod-ab-ukr. I've included those shapefiles here though. 


Citations: 
1. Raleigh, C., Linke, A., Hegre, H., & Karlsen, J. (2010). “Introducing ACLED: An armed conflict
location and event dataset: Special data feature”. Journal of Peace Research, 47(5), 651-660.
https://doi.org/10.1177/0022343310378914

2. Baddeley A, Rubak E, Turner R (2015). Spatial Point Patterns: Methodology and Applications with R. Chapman and Hall/CRC Press, London. https://www.routledge.com/Spatial-Point-Patterns-Methodology-and-Applications-with-R/Baddeley-Rubak-Turner/9781482210200/. 


For my purposes, in case I forget. Important FAQs from ACLED's terms of use. 

I want to use ACLED data in a dashboard I am building. Either single or multiple datasets will
be mapped/visualized and there will be an export functionality, allowing users to download
the raw data. Is this acceptable?
No, this is a violation of ACLED’s TOUs:
"All users are prohibited from ... 3. providing, permitting, or allowing direct access to any of
ACLED’s original/raw data or analysis to any other user.”

I would like to use ACLED data in an academic publication and am required to submit data
for replication to the journal. In this case, is it OK to share the raw data I used?
If you need to submit replication data alongside an academic manuscript, please share the specific
manipulated data you used in your project, which would necessarily not include all ACLED variables
(i.e. not a raw ACLED file). Submitting the original ACLED downloaded file is a violation of ACLED’s
TOUs because only ACLED can share original/raw ACLED data files:
"All users are prohibited from... 3. providing, permitting, or allowing direct access to any of
ACLED’s original/raw data or analysis to any other user.” Making ACLED data available on
platforms outside of ACLED not only undermines ACLED’s access model, but also creates a
context in which potential users of the data are not made aware of ACLED’s TOUs. They also
may encounter outdated versions of the data that has since been updated, as ACLED is a
living dataset.

I am working on a joint project and downloaded ACLED data for this. Can I share the data
within my team? What about with partners?
No one is individually permitted to share original/raw ACLED data, per ACLED’s TOUs:
"All users are prohibited from... 3. providing, permitting, or allowing direct access to any of
ACLED’s original/raw data or analysis to any other user.” In cases where an organization has
secured continued access to the data through a partnership with ACLED (see above),
members of that organization can share data with one another. If you have a partner on your
project outside of your organization (e.g. you work at Organization X, yet you are working
with Organization Y on a joint project together), your partner organization will need to
register to gain free access the data and establish a separate relationship with ACLED if they
need extended access; please do not share ACLED data with them, as this would be a TOU
violation.


