## 2026-07-28

**Error:** `Error in `met.brewer()`:`
**Fixes applied:** met.brewer() color count mismatch (Hiroshige palette insufficient for lollipop), manual scale color mismatch (6 categories but 4 colors provided), missing print() statements for crime lollipop and housing small multiples plots















## 2026-07-29

**Error:** `Error in `select()`:`
**Fixes applied:** Column name mismatch 'kjonn' vs actual column name in df1_dumbbell join; missing print() statement in plot-lollipop-change chunk














## 2026-07-30

**Data unavailable:** SSB tables 09171, 13863, 08484 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)













## 2026-07-30

**Error:** `Error in `if (!is.na(all_crime_label)) ...`:`
**Fixes applied:** Initialize all_crime_label before conditional use to prevent "argument is of length zero" error; add print() statements to plot chunks that were missing figure output












## 2026-07-31

**Data unavailable:** SSB tables 10949, 09186 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)











## 2026-07-31

**Data unavailable:** SSB tables 13764, 11503, 12508 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)










## 2026-07-31

**Error:** `Error in `mutate()`:`
**Fixes applied:** type coercion for Latest/Earliest (non-numeric), missing print() statements in plot-dumbbell-hardship and plot-area-hardship-national, palette size mismatch (11 regions but only 4 colors)









## 2026-08-01

**Data unavailable:** SSB tables 09788, 11653 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)








## 2026-08-01

**Data unavailable:** SSB tables 03013, 10634, 08771 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)







## 2026-08-01

**Data unavailable:** SSB tables 08800, 13760, 05803 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)






## 2026-08-01

**Data unavailable:** SSB tables 14365, 12349, 14651 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)





## 2026-08-01

**Data unavailable:** SSB tables 06265, 08381 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)




## 2026-08-05

**Data unavailable:** SSB tables 11573, 05110, 06512 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)



## 2026-08-05

**Data unavailable:** SSB tables 07221, 11386, 06988 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)


## 2026-08-05

**Error:** `Error in `geom_tile()`:`
**Fixes applied:** series_short not defined in plot-heatmap-balance chunk (created in plot-area-balance but not carried forward); missing print(p) statement in plot-heatmap-balance

