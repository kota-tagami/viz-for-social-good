library(tidyverse)
source("estat-api.R")

project_path <- "p01_She-cession"

save_name_ja <-
  "lfs_workers_month_ja.rds" %>%
  str_c(project_path, "Data", ., sep = "/")
save_name_en <-
  "lfs_workers_month_en.rds" %>%
  str_c(project_path, "Data", ., sep = "/")

stats_id <- "0002060003"
# https://www.e-stat.go.jp/stat-search/database?page=1&layout=datalist&stat_infid=000032060211&statdisp_id=0002060003


##=======================================================##
## >> Section No.1 <<
## >> Meta Information << 
##=======================================================##
res_meta <- 
  estat_api("getMetaInfo", list("statsDataId" = stats_id))

df_meta <- 
  res_meta %>% 
  pluck("GET_META_INFO", "METADATA_INF", "CLASS_INF", "CLASS_OBJ") %>% 
  tibble(data = .) %>% 
  hoist(
    data,
    var_id = "@id", var_lab = "@name", 
    class_data = "CLASS"
  ) %>% 
  mutate(
    named_list = class_data %>% 
      map_lgl(~ names(.) %>% is.null %>% isFALSE)
  ) %>% 
  {
    dt <- .
    
    dt1 <- 
      dt %>% 
      filter(named_list) %>% 
      hoist(
        class_data,
        val_id = "@code", val_lab = "@name",
        val_level = "@level", val_unit = "@unit"
      )
    
    dt2 <- 
      dt %>% 
      filter(!named_list) %>% 
      unnest_auto(class_data) %>% 
      hoist(
        class_data,
        val_id = "@code", val_lab = "@name",
        val_level = "@level", parent_code = "@parentCode"
      )
    
    bind_rows(dt1, dt2)
  } %>% 
  select(-c(named_list))


##=======================================================##
## >> Section No.2 <<
## >> Params << 
##=======================================================##
## 非農林業セクター

#-----------------------------#
# >> ind << #
#-----------------------------#
code_cat01 <- 
  df_meta %>% 
  filter(
    var_lab == "産業",
    val_lab == "非農林業",
  ) %>% 
  pull(val_id) %>% 
  str_c(collapse = ",")


#-----------------------------#
# >> gender << #
#-----------------------------#
code_cat02 <- 
  df_meta %>% 
  filter(
    var_lab == "性別",
    val_lab %in% c("男", "女")
  ) %>% 
  pull(val_id) %>% 
  str_c(collapse = ",")


#-----------------------------#
# >> employment << #
#-----------------------------#
code_cat03 <- 
  df_meta %>% 
  filter(
    var_lab == "従業上の地位",
    val_level %in% c("1", "2")
  ) %>% 
  pull(val_id) %>% 
  str_c(collapse = ",")


#-----------------------------#
# >> age << #
#-----------------------------#
code_cat04 <-
  df_meta %>% 
  filter(
    var_lab == "年齢階級",
    val_lab %in% c(
      "15歳以上", "15～64歳",
      "15～24歳", "25～34歳", "35～44歳",
      "45～54歳", "55～64歳", "65歳以上"
    ),
  ) %>% 
  pull(val_id) %>% 
  str_c(collapse = ",")


#-----------------------------#
# >> time << #
#-----------------------------#
code_time <- 
  df_meta %>% 
  filter(
    var_lab == "時間軸（月次）",
    val_lab %>% str_detect(str_c(c(2015:2021), collapse = "|")) 
  ) %>% 
  pull(val_id) %>% 
  str_c(collapse = ",")


##=======================================================##
## >> Section No.3 <<
## >> Stats Data << 
##=======================================================##
res_stats <- estat_api(
  "getStatsData",
  list(
    "statsDataId" = stats_id,
    "cdCat01" = code_cat01,
    "cdCat02" = code_cat02,
    "cdCat03" = code_cat03,
    "cdCat04" = code_cat04
  )
)

df_stats <- 
  res_stats %>% 
  pluck("GET_STATS_DATA", "STATISTICAL_DATA", "DATA_INF", "VALUE") %>% 
  tibble(data = .) %>% 
  unnest_auto(data) %>% 
  rename_with(
    ~ str_remove_all(., "\\@") %>% 
      str_replace_all("\\$", "value")
  ) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(-id) %>% 
  left_join(
    df_meta %>% 
      select(var_id, val_id, var_lab, val_lab),
    by = c("name" = "var_id", "value" = "val_id")
  ) %>% 
  mutate(
    across(where(is.character), stringi::stri_trans_nfkc),
    var_lab = if_else(is.na(var_lab), name, var_lab),
    val_lab = if_else(is.na(val_lab), value, val_lab)
  ) %>% 
  select(-c(name, value)) %>% 
  pivot_wider(names_from = var_lab, values_from = val_lab)


##=======================================================##
## >> Section No.4 <<
## >> Handling << 
##=======================================================##
df_stats_en <- 
  df_stats %>% 
  rename(
    tab = `表章項目`,
    ind = `産業`,
    gender = `性別`,
    empst = `従業上の地位`,
    age = `年齢階級`,
    area = `地域`,
    period = `時間軸(月次)`,
  ) %>% 
  mutate(
    across(where(is.character), factor),
    tab = tab %>% 
      fct_recode("population" = "実数(人口)"),
    ind = ind %>% 
      fct_recode(
        "Non-agriculture" = "非農林業"
      ),
    empst = empst %>% 
      fct_recode(
        "Workers" = "総数", 
        "Self-employed, family-business" = "自営業主・家族従業者",
        "Self-employed" = "自営業主",
        "Family-business" = "家族従業者",
        "Employees" = "雇用者"
      ) %>% 
      fct_relevel(
        "Workers", "Self-employed, family-business", 
        "Self-employed", "Family-business", "Employees"
      ),
    gender = gender %>% 
      fct_recode("Women" = "女", "Men" = "男") %>% 
      fct_relevel("Women", "Men"),
    age = age %>% 
      fct_recode(
        "15++" = "15歳以上",
        "15-64" = "15~64歳",
        "15-24" = "15~24歳",
        "25-34" = "25~34歳",
        "35-44" = "35~44歳",
        "45-54" = "45~54歳",
        "55-64" = "55~64歳",
        "65++" = "65歳以上"
      ) %>% 
      fct_relevel(
        "15++", "15-64", 
        "15-24", "25-34", "35-44", "45-54", "55-64",
        "65++"
      ),
    area = area %>% 
      fct_recode("All" = "全国"),
    year = period %>% 
      str_extract("^\\d{4}") %>% 
      as.numeric(),
    month = period %>% 
      str_extract("\\d{1,2}月$") %>% 
      str_remove_all("月") %>% 
      as.numeric(),
    date = lubridate::make_date(year, month, 1),
    value = value %>% 
      as.character() %>% 
      as.numeric(),
    value = value * 10000
  ) %>% 
  group_by(
    tab, ind, empst, gender, age, area, 
    period, year, month, date
  ) %>% 
  summarise(
    value = sum(value, na.rm = T),
    .groups = "drop"
  ) %>% 
  mutate(
    value = if_else(value == 0, NA_real_, value)
  )


##=======================================================##
## >> Section No.5 <<
## >> Save << 
##=======================================================##
write_rds(df_stats, save_name_ja)
write_rds(df_stats_en, save_name_en)

