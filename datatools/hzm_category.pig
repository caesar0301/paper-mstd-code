/*
Generate APP categories schema from raw Hangzhou mobile logs.

Input: raw HZ mobile logs.
Ouput: application categories schema.

By chenxm - chenxm35@gmail.com
*/
-- %declare input '0816.sample'

import 'hzm_commons.pig';

hzm = M_HZM_DATA('$input');

cats = foreach hzm generate total_category, category, sub_category1,
    sub_category2, sub_category3, sub_category4;
dcat = distinct cats;
ocat = order dcat by category;
--store ocat into '$output/catetory_all' using PigStorage(';');

brief  = foreach hzm generate com.piggybox.http.ExtractUrlHost(uri_main) as
    host, category, sub_category1, sub_category2;
brief = filter brief by category is not null and sub_category1 is not null
    and sub_category2 is not null;
dbrief = distinct brief;

hosts = foreach dbrief generate host, category;
hosts = distinct hosts;
cgrp = group hosts by category;
cat_host = foreach cgrp generate group, hosts.host;
store cat_host into '$output/category_hosts';

sc1 = foreach dbrief generate category, sub_category1;
sc1 = distinct sc1;
scgrp = group sc1 by sub_category1;
sc1_cat = foreach scgrp generate group, sc1.category;

sc2 = foreach dbrief generate category, sub_category2;
sc2 = distinct sc2;
scgrp = group sc2 by sub_category2;
sc2_cat = foreach scgrp generate group, sc2.category;

store sc1_cat into '$output/subcat1_classes';
store sc2_cat into '$output/subcat2_classes';