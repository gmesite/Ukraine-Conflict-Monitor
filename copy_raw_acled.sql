

COPY raw_data.raw_acled_data (
event_id_cnty,
event_date,
year,
time_precision,
disorder_type,
event_type,
sub_event_type,
actor1,
assoc_actor_1,
inter1,
actor2,
assoc_actor_2,
inter2,
interaction,
civilian_targeting,
iso,
region,
country,
admin1,
admin2,
admin3,
location_variable,
latitude,
longitude,
geo_precision,
source,
source_scale,
notes,
fatalities,
tags,
timestamp_variable
)
FROM 'E:\ACLED\2022-01-01-2023-07-29-Ukraine.csv' 
WITH (FORMAT CSV, HEADER);


