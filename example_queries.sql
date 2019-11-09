select count(*) as row_count from content;

select count(distinct url) as distinct_urls from content;

select round(avg(length(content))) as average_script_char_length from content;

select url, count(*) as count from content group by url order by count(*) desc;