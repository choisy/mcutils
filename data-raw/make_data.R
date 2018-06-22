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
devtools::use_data(to_latin, internal = TRUE, overwrite = TRUE)
