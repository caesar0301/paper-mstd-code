/*
Pig scripts to process Hangzhou Mobile datasets.
MCC: 460,
MNC: 00 02 07

Data Field description (contributed by WHY@OMNILAB):
frame: the unique ID of each log
ttime: the starT time of request.
record_file_index: log dumped file index
lac: location area code.
ci: cell ID
imsi: International mobile subscriber identity
mobile_type: the mobile agent type
success: (undetermined)
response_time: reqeust response latency
content_length: byte number of requested object
uri_main: host name
failure_cause: the reason of request failure
dest_ip: server IP address
timeout: flat indicating if the request fails for timeout
last_fileindex: ID of last log file
retransfer_count: number of retransfer tries
packets: number of transfered packets
tcp_retransfer_count: TCP retransfer tries
flush_count: (undetermined)
radio_status: status of network radio
status_code: HTTP status code
fall_line: (undetermined)
web_volume: byte number of transfered web object
e_gprs: (undetermined)
user_agent: HTTP user agent header
content_type: HTTP content type header
umts_tdd: if UMTS/TDD used
ul_teid: client tunnle ID
dl_teid: desination tunnle ID
rad_type: Radio Access Type
suspend_time: (undetermined)
resume_time: (undetermined)
dtime: (maybe) log output time
sgsn_id: ID of GPRS core network
rnc_id: ID of Radio Network Controller
wap12: wap protocol
bsc_ip: IP of Base Station Controller
dest_port: server TCP port
sgsn_ip: IP of GPRS core network
user_ip_value: client IP address
total_catetory: (undetermined)
sub_category1: Classification to different service providers, e.g. netease
catetory: The TOP classification, e.g. video, music.
sub_category2 (undetermined rule)
uri: URI of HTTP header
sub_category3: (undetermined rule)
os: operating system of clients
sub_category4: (undetermined rule)
answer_time: (undetermined)
clientsoft: (undetermined)

By chenxm
chenxm35@gmail.com
*/


-- User regex to avoid other commas in string like UA
-- Confusing fields with commas:
-- mobile_type, user_agent, uri
-- This regex is not sufficient. I use pyspark to leave these out.
DEFINE M_HZM_DATA(input_log) RETURNS hzm {
    $hzm = load '$input_log' using
    com.piggybox.loader.STLRegex(',(?!\\s+|9[AB]|iPhone\\d+|\\d+\\.\\d+)') as (
        frame:chararray,
        ttime:chararray,
        record_file_index:chararray,
        LAC:chararray,
        CI:chararray,
        IMSI:chararray,
        mobile_type:chararray,
        success:chararray,
        response_time:chararray,
        content_length:chararray,
        uri_main:chararray,
        failure_cause:chararray,
        dest_ip:chararray,
        timeout:chararray,
        last_fileindex:chararray,
        retransfer_count:chararray,
        packets:chararray,
        tcp_retransfer_count:chararray,
        flush_count:chararray,
        radio_status:chararray,
        status_code:chararray,
        fall_line:chararray,
        web_volume:chararray,
        e_gprs:chararray,
        user_agent:chararray,
        content_type:chararray,
        umts_tdd:chararray,
        ul_teid:chararray,
        dl_teid:chararray,
        rad_type:chararray,
        suspend_time:chararray,
        resume_time:chararray,
        dtime:chararray,
        sgsn_id:chararray,
        rnc_id:chararray,
        wap12:chararray,
        bsc_ip:chararray,
        dest_port:chararray,
        sgsn_ip:chararray,
        user_ip_value:chararray,
        total_category:chararray,
        sub_category1:chararray,
        category:chararray,
        sub_category2:chararray,
        URI:chararray,
        sub_category3:chararray,
        OS:chararray,
        sub_category4:chararray,
        answer_time:chararray,
        client_soft:chararray
        );
    -- Skip file header
    $hzm = filter $hzm by frame != 'FRAME';
};

-- Load ETLed datasets
DEFINE M_HZM_DATA_ETLed(input_log) RETURNS hzm_clean {
    $hzm_clean = load '$input_log' using PigStorage('\t') as (
        ttime:double,
        dtime:double,
        BS:long,
        IMSI:chararray,
        mobile_type:chararray,
        dest_ip:long,
        dest_port:int,
        success:long,
        failure_cause:chararray,
        response_time:long,
        host:chararray,
        content_length:long,
        retransfer_count:long,
        packets:long,
        status_code:int,
        web_volume:long,
        content_type:chararray,
        UA:chararray,
        is_mobile:int,
        e_gprs:int,
        umts_tdd:int,
        ICP:long,
        SC:chararray,
        URI:chararray,
        OS:chararray,
        LON:double,
        LAT:double
        );
};
