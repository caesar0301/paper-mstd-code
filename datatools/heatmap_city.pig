/*
Generate dynamic mobile traffic map (i.e. heapmap) from ETLed traffic,
also including:
    Filter Hangzhou city data;
    Generate basic dimensions of data;
    Generate traffic map at BS granularity;

Input: ETLed mobile logs.
Output: heatmap of cellular traffic.

By chenxm - chenxm35@gmail.com
*/
-- %DECLARE input '../data/sample.clean.tiny.txt';
-- %DECLARE output 'output';

import 'hzm_commons.pig';
DEFINE DoubleToString com.piggybox.converter.DoubleToString;

-- Macro to draw heatmap of traffic
DEFINE DRAW_HEATMAP(logs) RETURNS heatmap{
    selected = foreach $logs generate
        (long) ttime/3600 as thour,
        BS,
        IMSI,
        packets,
        web_volume;
    ct_grp = group selected by (BS, thour);
    $heatmap = foreach ct_grp{
        unique_users = distinct selected.IMSI;
        generate flatten(group),
        SUM(selected.packets) as packets,
        SUM(selected.web_volume) as bytes,
        COUNT(unique_users) as users;
    }
};

-- Filter Hangzhou data
hzm = M_HZM_DATA_ETLed('$input');

-- Filter Hangzhou data merely
hangzhou = filter hzm by LON >= 120.0517 and LON <= 120.3489 and
    LAT >= 30.1341 and LAT <= 30.4833;
hangzhou_plain = foreach hangzhou generate
    DoubleToString(ttime, '%.3f') as ttime:chararray,
    DoubleToString(dtime, '%.3f') as dtime:chararray,
    BS ..;
store hangzhou_plain into '$output/hangzhou';

-- Draw heatmap of the entire dataset
heatmap_hz = DRAW_HEATMAP(hangzhou);
store heatmap_hz into '$output/heatmap.hz';

-- Basic statistic dimensions
hzh_all_grp = group hangzhou all;
basic_dim = foreach hzh_all_grp {
    unique_user = distinct hangzhou.IMSI;
    unique_bs = distinct hangzhou.BS;
    total_packets = SUM(hangzhou.packets) / 1024; -- Kp
    total_logs = COUNT(hangzhou);
    total_bytes = SUM(hangzhou.web_volume) / 1024 / 1024; -- MB
    generate
    COUNT(unique_user) as user_count,
    COUNT(unique_bs) as bs_count,
    total_logs,
    total_packets,
    total_bytes;
}
store basic_dim into '$output/basics.hz';

-- Draw traffic heatmap of Hangzhou of eight full-days
hangzhou_fd8 = filter hangzhou by
    -- From 2012/08/19 to 2012/08/26, eight full day
    ttime >= 1345305600 and
    ttime < 1345996800;
hangzhou_heatmap_fd8 = DRAW_HEATMAP(hangzhou_fd8);
store hangzhou_heatmap_fd8 into '$output/heatmap.hz.f8';