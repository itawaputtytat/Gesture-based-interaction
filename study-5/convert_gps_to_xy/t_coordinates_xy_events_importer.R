dat_temp <- readxl::read_xlsx(file.choose(), na = "NA")

dbWriteTable(get(dbFindConnObj("Study-5")), "t_coordinates_xy_events", dat_temp, overwrite = T, row.names = F)
