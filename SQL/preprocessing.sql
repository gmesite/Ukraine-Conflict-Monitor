-- Active: 1688081208223@@127.0.0.1@5432@professional@postgis


-- There are a few observations, which don't lie on a particular administrative boundary in the ACLED dataset, 
-- these observations lie in the Black Sea and aren't events that occur on land, so I'm going to remove them.

CREATE TABLE ukraine_war_analysis.copy_raw_acled_data_temp AS
SELECT * FROM ukraine_war_analysis.copy_raw_acled_data
WHERE admin1 IS NOT NULL;

DROP TABLE copy_raw_acled_data;

ALTER TABLE copy_raw_acled_data_temp RENAME TO copy_raw_acled_data;

-- Performing accurate spatial data analysis requires thoughtful consideration of each of our
-- datasets spatial reference system. 
SELECT postgis.ST_SRID(geom) FROM ukraine_war_analysis.ukr_bndy_level_0;

SELECT postgis.ST_SRID(geom) FROM ukraine_war_analysis.ukr_bndy_level_1;

SELECT postgis.ST_SRID(geom) FROM ukraine_war_analysis.ukr_bndy_level_2;


-- We're going to change it to WGS84, a commonly used reference system. First we need to know the geometry type we're working with, and then
-- we'll alter the table
ALTER TABLE ukraine_war_analysis.ukr_bndy_level_0
ALTER COLUMN geom TYPE postgis.geometry(MULTIPOLYGON, 4326)
USING postgis.ST_SetSRID(geom, 4326);

ALTER TABLE ukraine_war_analysis.ukr_bndy_level_1
ALTER COLUMN geom TYPE postgis.geometry(MULTIPOLYGON, 4326)
USING postgis.ST_SetSRID(geom, 4326);

ALTER TABLE ukraine_war_analysis.ukr_bndy_level_2
ALTER COLUMN geom TYPE postgis.geometry(MULTIPOLYGON, 4326)
USING postgis.ST_SetSRID(geom, 4326);

-- Checking to see whether or not the SRIDs are changed
SELECT postgis.ST_SRID(geom) FROM ukraine_war_analysis.ukr_bndy_level_0;

SELECT postgis.ST_SRID(geom) FROM ukraine_war_analysis.ukr_bndy_level_1;

SELECT postgis.ST_SRID(geom) FROM ukraine_war_analysis.ukr_bndy_level_2;




-- The administrative boundaries recorded by ACLED are not textually the same as the records from
-- our ukr_bndy_level_1 data, so I'm creating a column 'admin1' with exactly matching records. 
-- Then, one of the datasets does not identify Sevastopol as an adminsitrative boundary but
-- rather includes it within the boundary of Crimea, so we are merging Crimea and Sevastopol.
ALTER TABLE ukraine_war_analysis.ukr_bndy_level_1
ADD COLUMN admin1 VARCHAR(50);

UPDATE ukraine_war_analysis.ukr_bndy_level_1
SET admin1 =
    CASE 
        WHEN (adm1_en IN ('Autonomous Republic of Crimea', 'Sevastopol')) THEN 'Crimea'  
        WHEN (adm1_en IN ('Cherkaska')) THEN 'Cherkasy'
        WHEN (adm1_en IN ('Chernihivska')) THEN 'Chernihiv'
        WHEN (adm1_en IN ('Chernivetska')) THEN 'Chernivtsi'
        WHEN (adm1_en IN ('Dnipropetrovska')) THEN 'Dnipropetrovsk'
        WHEN (adm1_en IN ('Donetska')) THEN 'Donetsk'
        WHEN (adm1_en IN ('Ivano-Frankivska')) THEN 'Ivano-Frankivsk'
        WHEN (adm1_en IN ('Kharkivska')) THEN 'Kharkiv'
        WHEN (adm1_en IN ('Khersonska')) THEN 'Kherson'
        WHEN (adm1_en IN ('Khmelnytska')) THEN 'Khmelnytskyi'
        WHEN (adm1_en IN ('Kirovohradska')) THEN 'Kirovograd'
        WHEN (adm1_en IN ('Kyiv')) THEN 'Kyiv City'
        WHEN (adm1_en IN ('Kyivska')) THEN 'Kyiv'
        WHEN (adm1_en IN ('Luhanska')) THEN 'Luhansk'
        WHEN (adm1_en IN ('Lvivska')) THEN 'Lviv'
        WHEN (adm1_en IN ('Mykolaivska')) THEN 'Mykolaiv'
        WHEN (adm1_en IN ('Odeska')) THEN 'Odesa'
        WHEN (adm1_en IN ('Poltavska')) THEN 'Poltava'
        WHEN (adm1_en IN ('Rivnenska')) THEN 'Rivne'
        WHEN (adm1_en IN ('Sumska')) THEN 'Sumy'
        WHEN (adm1_en IN ('Ternopilska')) THEN 'Ternopil'
        WHEN (adm1_en IN ('Vinnytska')) THEN 'Vinnytsia'
        WHEN (adm1_en IN ('Volynska')) THEN 'Volyn'
        WHEN (adm1_en IN ('Zakarpatska')) THEN 'Zakarpattia'
        WHEN (adm1_en IN ('Zaporizka')) THEN 'Zaporizhia'
        WHEN (adm1_en IN ('Zhytomyrska')) THEN 'Zhytomyr'
    END;


-- Create a table to store unioned geometry (geometry containing two administration zones named Crimea, one geometry that is actually Crimea and the other
-- Sevastopol but is named Crimea)
CREATE TABLE unioned_geometries (
    admin1 VARCHAR(50),
    geom postgis.geometry(MULTIPOLYGON, 4326)
);

-- Inserting unioned geometry into the table
INSERT INTO unioned_geometries (admin1, geom)
SELECT 
    'Crimea',
    postgis.ST_Union(geom) 
FROM ukraine_war_analysis.ukr_bndy_level_1
WHERE admin1 = 'Crimea';


-- Deleting the two Crimeas from our ukr_bndy_level_1 table
DELETE FROM ukraine_war_analysis.ukr_bndy_level_1
WHERE admin1 = 'Crimea';

-- Inserting the unioned geometry into our ukr_bndy_level_1 table
INSERT INTO ukraine_war_analysis.ukr_bndy_level_1 (admin1, geom) 
(SELECT * FROM ukraine_war_analysis.unioned_geometries);

-- Setting the gid of the new row containing the unioned geometry
ALTER TABLE ukraine_war_analysis.ukr_bndy_level_1 
DROP COLUMN gid,
ADD COLUMN gid bigserial;

-- Dropping unioned geometries table
DROP TABLE ukraine_war_analysis.unioned_geometries;

-- Joining our ACLED data with our geometries table
CREATE TABLE ukraine_war_analysis.acled_data AS
SELECT table1.*, table2.geom
FROM ukraine_war_analysis.copy_raw_acled_data AS table1 
JOIN ukraine_war_analysis.ukr_bndy_level_1 AS table2
USING (admin1);

-- Removing non-informative columns
ALTER TABLE ukraine_war_analysis.acled_data
DROP COLUMN event_id_cnty,
DROP COLUMN iso,
DROP COLUMN region,
DROP COLUMN country,
DROP COLUMN fatalities,
DROP COLUMN tags,
DROP COLUMN timestamp_variable;


-- Create unique record ID as PRIMARY KEY
ALTER TABLE ukraine_war_analysis.acled_data
ADD COLUMN record_id bigserial CONSTRAINT record_key PRIMARY KEY;



-- I also may want to do some monthly comparisons of various events, and I may need to aggregate observations based on month and year
ALTER TABLE ukraine_war_analysis.acled_data
ADD COLUMN month INTEGER;

UPDATE ukraine_war_analysis.acled_data
SET month = EXTRACT(MONTH FROM event_date);


-- Now I want to create a dataset for which point coordinates are the geometry, not a polygonal boundary geometry. 
CREATE TABLE ukraine_war_analysis.acled_data_coordinates AS
SELECT 
    record_id,
    event_date,
    month,
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
    admin1,
    admin2,
    admin3,
    location_variable,
    latitude,
    longitude,
    geo_precision,
    source,
    source_scale,
    notes    
FROM ukraine_war_analysis.acled_data;

-- Adding the new geometry column
ALTER TABLE ukraine_war_analysis.acled_data_coordinates
ADD COLUMN geom postgis.geometry(POINT, 4326);

-- Adding the data for the geometry column, the lat and long coordinates
UPDATE ukraine_war_analysis.acled_data_coordinates
SET geom = postgis.ST_SetSRID(postgis.ST_MakePoint(longitude, latitude), 4326);

-- Dropping the longitude and latitude columns that were used to derive the geometry column from the directly above code
ALTER TABLE ukraine_war_analysis.acled_data_coordinates
DROP COLUMN longitude,
DROP COLUMN latitude;

-- We're changing the CRS from WGS84 lat long to one specifically designed for the area of Ukraine (6381)
-- Here we're checking to see if it exists
SELECT * FROM postgis.spatial_ref_sys
WHERE srid = 6381;

-- Transforming the SRID
ALTER TABLE ukraine_war_analysis.acled_data_coordinates
ALTER COLUMN geom TYPE postgis.geometry(Point, 6381)
USING postgis.ST_SetSRID(geom, 6381);

-- Transforming the coordinates
UPDATE ukraine_war_analysis.acled_data_coordinates
SET geom = postgis.ST_Transform(geom, 6381);

-- Checking information on this coordinate SYSTEM
SELECT srid, srtext, proj4text, auth_srid, auth_name
FROM postgis.spatial_ref_sys
WHERE srid = 6381;

-- Verifying the changes
SELECT postgis.ST_SRID(geom) FROM ukraine_war_analysis.acled_data_coordinates;

-- We need another table that just has the coordinates in two separate columns, X and Y
CREATE TABLE ukraine_war_analysis.acled_data_xy_coords  AS
SELECT * FROM ukraine_war_analysis.acled_data_coordinates;

-- Adding two columns X and Y that represent the lat and long coordinates
-- 
ALTER TABLE ukraine_war_analysis.acled_data_xy_coords
ADD COLUMN x DOUBLE PRECISION,
ADD COLUMN y DOUBLE PRECISION;

UPDATE ukraine_war_analysis.acled_data_xy_coords 
SET
    x = postgis.ST_X(acled_data_xy_coords.geom), 
    y = postgis.ST_Y(acled_data_xy_coords.geom);

-- Dropping the unnecessary column
ALTER TABLE ukraine_war_analysis.acled_data_xy_coords
DROP COLUMN geom;

-- We want another time index compatible with spatial-temporal point pattern
-- analysis packages in R, so we're adding a column to fix that.
ALTER TABLE ukraine_war_analysis.acled_data_xy_coords
ADD COLUMN time_index integer;

UPDATE ukraine_war_analysis.acled_data_xy_coords
SET time_index = subquery.rank
FROM (
    SELECT event_date, DENSE_RANK() OVER (ORDER BY event_date) AS rank
    FROM ukraine_war_analysis.acled_data_xy_coords
) AS subquery
WHERE ukraine_war_analysis.acled_data_xy_coords.event_date = subquery.event_date;



-- Now we need to get count data. 
CREATE TABLE acled_data_count_day AS
(SELECT event_date, admin1, sub_event_type, COUNT(*) AS count_day
FROM ukraine_war_analysis.acled_data
GROUP BY event_date, admin1, sub_event_type);

-- Now I want to get count data based off months
CREATE TABLE acled_data_count_month AS
(SELECT year, month, admin1, sub_event_type, COUNT(*) as count_month
FROM ukraine_war_analysis.acled_data
GROUP BY year, month, admin1, sub_event_type);


-- I want a rasterize my geometry COLUMNS
CREATE TABLE ukraine_war_analysis.ukr_bndy_level_1_raster AS
(SELECT postgis.ST_AsRaster(geom, 400, 400, NULL, NULL, 0) AS raster
FROM ukraine_war_analysis.ukr_bndy_level_1);

ALTER TABLE ukraine_war_analysis.ukr_bndy_level_1_raster
ADD COLUMN gid bigserial;

ALTER TABLE ukraine_war_analysis.ukr_bndy_level_1_raster
ADD COLUMN admin1 VARCHAR(30);

UPDATE ukraine_war_analysis.ukr_bndy_level_1_raster
SET admin1 = subquery.admin1
FROM (
    SELECT admin1, gid FROM ukraine_war_analysis.ukr_bndy_level_1
) AS subquery
WHERE ukraine_war_analysis.ukr_bndy_level_1_raster.gid = subquery.gid;



















