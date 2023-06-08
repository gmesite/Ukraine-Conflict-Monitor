# Ukraine-Conflict-Monitor
A Shiny dashboard designed for the exporatory analysis of data from ACLED. The data is an event-based dataset, in spreadsheet form, where each row represents an event. The columns have spatial and temporal information, as well as event type, actors involved, etc. To access their data, visit the “Armed Conflict Location & Event Data Project (ACLED) website at www.acleddata.com.”. To see the dashboard in action see https://www.youtube.com/watch?v=rkb_C4Q0X20. Unforunately the dashboard is not available for viewing online but will be available in September. 


I have not included the ACLED data in github, as it is against ACLED's terms of service. However, spatial data for Ukraine's subnational administrative boundaries can be downloaded 
for free at https://data.humdata.org/dataset/cod-ab-ukr. I've included those shapefiles here though. 

![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/4891e410-3fce-41f9-b400-3dddfcdf607c)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/4436d47c-c3d0-452e-b8fc-af40a3066214)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/558ea35e-9142-4632-9fe6-fa2db4fee255)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/76c783d1-83c9-4ea1-8630-e8fed7644784)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/e5d57a45-11cf-41b6-879f-d5de0e1d727b)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/ff47dce0-2ba5-4053-a48a-f54010a1dcd6)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/7860f9f8-194d-4556-b4b9-9ffb38abf8f8)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/e3f4916b-8d53-4703-a5a0-edd52221c05f)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/396f7ce9-e104-49e8-8e36-6afc97ef7c21)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/ad9515ee-7b9a-4fdb-9e41-73781882a8a3)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/0f64e862-ba33-4888-aaf9-9c8139bb3e2e)
![image](https://github.com/EliGacasan/Ukraine-Conflict-Monitor/assets/110142627/fd87b766-9506-41f0-858f-f27b855beba4)


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


