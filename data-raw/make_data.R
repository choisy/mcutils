# Vietnamese letters -----------------------------------------------------------
tmp <- read.table("data-raw/vietnamese_letters.txt")
latin_letters <- c(rep("A", 17),
                       "D",
                   rep("E", 11),
                   rep("O", 17),
                   rep("U", 11),
                   rep("I",  5),
                   rep("a", 17),
                       "d",
                   rep("e", 11),
                   rep("o", 17),
                   rep("u", 11),
                   rep("i",  5))
to_latin <- data.frame(tmp, latin_letters)


# Geographical projections -----------------------------------------------------
proj0 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
projVN <- sp::CRS("+proj=utm +zone=48 +ellps=WGS84 +units=m +no_defs")


# Dictionary -------------------------------------------------------------------
dictionary <- setNames(data.frame(matrix(c(
                                           "Ba ria - Vung tau",  "Ba Ria - Vung Tau",
                                           "Bac can"          ,  "Bac Kan",
                                           "Ha noi II"        ,  "Ha Tay",
                                           "Hanoi"            ,  "Ha Noi",
                                           "Soc T rang"       ,  "Soc Trang",
                                           "SOn ia"           ,  "Son La",
                                           "Son ia"           ,  "Son La",
                                           "Thua Thien - Hue" ,  "Thua Thien Hue",
                                           "TP Ho Chi Minh"   ,  "Ho Chi Minh",
                                           "Tp. Ho Chi Minh"  ,  "Ho Chi Minh",
                                           "Vinh iong"        ,  "Vinh Long",
                                           "Vung Tau - Ba Ria",  "Ba Ria - Vung Tau"
                                          ), ncol = 2, byrow = TRUE),
                                  stringsAsFactors = FALSE), c("from", "to"))


# Saving to disk (internal) ----------------------------------------------------
devtools::use_data(to_latin, dictionary, internal = TRUE, overwrite = TRUE)


# Saving to disk (external) ----------------------------------------------------
devtools::use_data(proj0, projVN, overwrite = TRUE)
