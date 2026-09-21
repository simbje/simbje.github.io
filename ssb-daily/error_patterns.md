## 2026-09-08

**Data unavailable:** SSB tables 08268, 05803 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)















## 2026-09-08

**Data unavailable:** SSB tables 08771, 11327, 09363 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)














## 2026-09-08

**Error:** `Error in `loadNamespace()`:`
**Fixes applied:** removed janitor dependency (janitor::clean_names() call), initialized df2_lollipop properly to avoid undefined variable error













## 2026-09-11

**Error:** `Error in `if (ens$.mean > last_obs) ...`:`
**Fixes applied:** missing value in if-condition due to NA in ens$.mean, null-guard logic incomplete












## 2026-09-14

**Data unavailable:** SSB tables 14620, 13635 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)











## 2026-09-14

**Error:** `Error in `rename()`:`
**Fixes applied:** Column name mismatch in df2 pivot_wider (Råolje og naturgass, eksport doesn't exist after filtering), sprintf format string error (%d for numeric oil_share), missing print() statements for plots










## 2026-09-17

**Data unavailable:** SSB tables 06988, 14472, 09429 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)









## 2026-09-17

**Error:** `Error in parse(text = input): <text>:14:3: unexpected 'if'`
**Fixes applied:** Parse error in plot-migration-slope chunk (malformed if_else statement with unclosed parenthesis); Missing print(p2) statement in plot-migration-slope chunk causing no figure output








## 2026-09-18

**Data unavailable:** SSB tables 03013 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)







## 2026-09-18

**Data unavailable:** SSB tables 13760 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)






## 2026-09-18

**Data unavailable:** SSB tables 06090, 11386, 01223 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)





## 2026-09-18

**Error:** `Error in `met.brewer()`:`
**Fixes applied:** met.brewer color palette count exceeded discrete limit (reduced from 4 to 3), missing print() statements in three plot chunks, color palette initialization errors in emissions lollipop




## 2026-09-21

**Data unavailable:** SSB tables 13863, 12315, 09190 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)



## 2026-09-21

**Data unavailable:** SSB tables 14366, 13771 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)


## 2026-09-21

**Error:** `Error in `mutate()`:`
**Fixes applied:** mutate() error in df3_dumbbell due to column name case mismatch (Latest/Earliest vs Earliest/Latest), missing print() statements in plot-dumbbell-employed and plot-ridgeline-employed chunks

