## 2026-04-21

**Error:** `Error in `filter()`: ! Column `type justering` not found in `.data`.`
**Fixes applied:** (backfilled from freeze cache — not auto-fixed)











## 2026-04-25

**Error:** `Error in `scale_y_continuous()`: ! Discrete value supplied to a continuous scale.`
**Fixes applied:** (backfilled from freeze cache — not auto-fixed)











## 2026-04-28

**Error:** `Error in `mutate()`: ! factor level [2] is duplicated`
**Fixes applied:** (backfilled from freeze cache — not auto-fixed)











## 2026-05-03

**Error:** `Error in `mutate()`: ! there is no package called 'zoo'`
**Fixes applied:** (backfilled from freeze cache — not auto-fixed)











## 2026-05-07

**Error:** `Error: ! object 'df1_top' not found`
**Fixes applied:** (backfilled from freeze cache — not auto-fixed)










## 2026-05-08

**Error:** `Missing figure: plot-vacancy-area`
**Fixes applied:** Added explicit print() calls to all three missing plot chunks (plot-vacancy-area, plot-dumbbell, plot-labourforce-ridgeline) to ensure ggplot objects render as figures










## 2026-06-02

**Error:** `Missing figure: plot-vacancies-small-multiples`
**Fixes applied:** Added explicit print(p) statements to all five plot chunks to ensure ggplot objects are rendered; all plot chunks now have unconditional print() after the ggplot() assignment.









## 2026-06-08

**Data unavailable:** SSB tables unknown returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)








## 2026-06-08

**Data unavailable:** SSB tables 13839, 06083, 13793 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)







## 2026-06-09

**Data unavailable:** SSB tables 14365, 08669, 13835 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)






## 2026-06-09

**Data unavailable:** SSB tables 09190, 08307, 05110 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)





## 2026-06-09

**Error:** `Error in `filter()`:`
**Fixes applied:** Initialize df1_by_ctry before conditional block to prevent 'object not found' error; remove duplicate factor levels in df2_recent_plot; add explicit print() statements to ensure plot output




## 2026-06-09

**Data unavailable:** SSB tables 11503, 06921 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)



## 2026-06-09

**Data unavailable:** SSB tables 11386, 08800 returned no data or API error
**Fixes applied:** none (post scrapped — data-level issue, not a code bug)


## 2026-06-09

**Error:** `Error in `mutate()`:`
**Fixes applied:** Non-numeric argument error in mutate() for dumbbell chart (val_first and val_last were characters, not numeric), missing print() statement for dumbbell plot

