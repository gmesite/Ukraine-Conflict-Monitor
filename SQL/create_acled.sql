-- Active: 1688081208223@@127.0.0.1@5432@professional





CREATE TABLE raw_data.raw_acled_data (
    event_id_cnty VARCHAR(15) NOT NULL,
    event_date DATE NOT NULL,
    year INTEGER,
    time_precision INTEGER,
    disorder_type VARCHAR(34),
    event_type VARCHAR(26),
    sub_event_type VARCHAR(35),
    actor1 VARCHAR(69),
    assoc_actor_1 VARCHAR(168),
    inter1 INTEGER,
    actor2 VARCHAR(69),
    assoc_actor_2 VARCHAR(204),
    inter2 INTEGER,
    interaction INTEGER,
    civilian_targeting VARCHAR(18),
    iso INTEGER,
    region VARCHAR(6),
    country VARCHAR(7),
    admin1 VARCHAR(15),
    admin2 VARCHAR(21),
    admin3 VARCHAR(31),
    location_variable VARCHAR(33),
    latitude DECIMAL,
    longitude DECIMAL,
    geo_precision INTEGER,
    source TEXT,
    source_scale VARCHAR(23),
    notes TEXT,
    fatalities INTEGER,
    tags TEXT,
    timestamp_variable NUMERIC
);













