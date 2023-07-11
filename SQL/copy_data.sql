-- Active: 1688081208223@@127.0.0.1@5432@professional@ukraine_war_analysis



-- Here I'm putting the raw acled data into another SCHEMA 'ukraine_war_analysis' where I'll store all my preprocessed data.
-- Also I'm adding a copy of acled_data to 'ukraine_war_analysis'
CREATE SCHEMA IF NOT EXISTS ukraine_war_analysis;

CREATE TABLE IF NOT EXISTS ukraine_war_analysis.copy_raw_acled_data AS
SELECT * FROM raw_data.raw_acled_data;


-- Adding copy of several spatial data files from the raw_data SCHEMA
CREATE TABLE IF NOT EXISTS ukraine_war_analysis.ukr_bndy_level_0 AS
SELECT * FROM raw_data.ukr_admbnda_adm0_sspe_20230201;

CREATE TABLE IF NOT EXISTS ukraine_war_analysis.ukr_bndy_level_1 AS
SELECT * FROM raw_data.ukr_admbnda_adm1_sspe_20230201;


CREATE TABLE IF NOT EXISTS ukraine_war_analysis.ukr_bndy_level_2 AS
SELECT * FROM raw_data.ukr_admbnda_adm2_sspe_20230201;
































