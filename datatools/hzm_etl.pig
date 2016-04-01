/*
ETL the raw mobile logs of Hangzhou.

Usage:
    pig
    -param mobilelog=data/mobile_utf8
    -param cellmap=data/hzwz_cell_map.clean.txt
    -param output=hzm/etled
    hzm_etl.pig

Input: Raw data of HZ mobile log
Output: ETLed clean logs.

By chenxm - chenxm35@gmail.com
*/
-- For debugging
-- %DECLARE mobilelog '../data/sample.raw.txt';
-- %DECLARE cellmap '../data/hzwz_cell_map.clean.txt';
-- %DECLARE output 'output';

import 'hzm_commons.pig';
define MD5 datafu.pig.hash.MD5('base64');
define UAClassify com.piggybox.http.UserAgentClassify;
define ParseTimeString com.piggybox.omnilab.ParseTimeString;

-- Load raw logs
hzm = M_HZM_DATA('$mobilelog');

-- Filter fields to remove junks.
hzm_selected = foreach hzm generate
    ParseTimeString(ttime) as ttime:float,
    ParseTimeString(dtime) as dtime:float,
    (LONG)LAC*1000000+ (LONG)CI as BS:long, -- base station ID
    IMSI as IMSI:chararray,
    mobile_type,
    dest_ip as dest_ip:long,
    dest_port as dest_port:long,
    success as success:int,
    failure_cause,
    response_time as response_time:long,
    uri_main as host:chararray,
    content_length as content_length:long,
    retransfer_count as retransfer_count:long,
    packets as packets:long,
    STRSPLIT(TRIM(status_code), ' ', 2).$0 as status_code:long,
    web_volume as web_volume:long,
    content_type,
    (user_agent is null ? user_agent : MD5(user_agent)) as UA:chararray,
    UAClassify(user_agent) as is_mobile:int,
    e_gprs as e_gprs:int,
    umts_tdd as umts_tdd:int,
    sub_category1 as ICP:long, -- Internet Content Provider
    category as SC:chararray, -- Service type
    URI, OS;

-- Read base station location data
-- Loading cell station locations
cell_map = load '$cellmap' as
        (LAC:long, CI:long, LON:float, LAT:float, desp:chararray);
cellmap_light = foreach cell_map generate
    LAC*1000000+CI as BS:long, LON, LAT;

-- Add location data to mobile logs
hzm_selected = join hzm_selected by BS, cellmap_light by BS;
hzm_selected = foreach hzm_selected generate ttime .. OS, LON, LAT;

/*********************************************
** Filter invalid records
** Notice this is not accurate as for the confusing commans in mobile_type,
** user_agent, and URI.
*********************************************/
hzm_clean = filter hzm_selected by dtime is not null;
store hzm_clean into '$output/hzm_clean';

/*********************************************
** Extract all mobilities without leave-out.
*********************************************/
hzm_mobility = foreach hzm_selected generate ttime, IMSI, BS, LON, LAT;
hzm_mobility = filter hzm_mobility by
    IMSI is not null and
    STARTSWITH(IMSI, '460');
hzm_mobility = order hzm_mobility by IMSI, ttime;
store hzm_mobility into '$output/mobility';