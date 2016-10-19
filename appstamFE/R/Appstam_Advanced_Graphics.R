library(ggplot2)
plot <- function(vvv__1, vvv__2, vvv__3, vvv__4 = NULL, vvv__5 = NULL, vvv__6 = NULL, vvv__7 = NULL, vvv__8 = NULL, vvv__9 = NULL, vvv__10 = NULL, 
    vvv__11 = NULL, vvv__12 = NULL, vvv__13 = ".", vvv__14 = ".") {
    vvv__15 <- NULL
    if (vvv__13 != "." | vvv__14 != ".") {
        vvv__15 <- facet_grid(eval(parse(text = paste(vvv__13, "~", vvv__14, collapse = ""))))
    }
    vvv__16 <- NULL
    vvv__17 <- NULL
    vvv__18 <- NULL
    arg_aes_string <- list()
    arg_aes_string["x"] <- vvv__3
    arg_aes_string["y"] <- vvv__4
    arg_aes_string["color"] <- vvv__5
    vvv__19 <- NULL
    arg_labels = list()
    arg_labels["x"] <- vvv__11
    arg_labels["y"] <- vvv__12
    arg_labels["title"] <- vvv__10
    if (length(arg_labels != 0)) {
        vvv__19 <- do.call(labs, arg_labels)
    }
    if (vvv__2 == "point") {
        vvv__20 <- geom_point()
    }
    else if (vvv__2 == "bar") {
        vvv__20 <- geom_bar()
        arg_aes_string["y"] <- NULL
    }
    else if (vvv__2 == "density") {
        vvv__20 <- geom_density()
        arg_aes_string["y"] <- NULL
    }
    else if (vvv__2 == "line") {
        vvv__20 <- geom_line()
    }
    else if (vvv__2 == "boxplot") {
        vvv__20 <- geom_boxplot()
    }
    if (!is.null(vvv__9)) {
        vvv__16 <- geom_smooth()
    }
    label_1 <- paste(strsplit("                     senihcaM scitsitatS deilppA", split = NULL)[[1]][length(strsplit("                     senihcaM scitsitatS deilppA", 
        split = NULL)[[1]]):1], collapse = "")
    label_2 <- paste("ap", paste(strsplit("c.matsp", split = NULL)[[1]][length(strsplit("c.matsp", split = NULL)[[1]]):1], collapse = ""), 
        "om", sep = "")
    lizdate <- Sys.Date() - 14
    if (unclass(difftime(lizdate, Sys.Date(), units = "weeks"))[1] < -1) {
        vvv__17 <- annotate("text", x = Inf, y = -Inf, label = label_1, hjust = 1.1, vjust = -1.1, col = "white", cex = 6, fontface = "bold", 
            alpha = 0.8)
        vvv__18 <- annotate("text", x = Inf, y = -Inf, label = label_2, hjust = 1.1, vjust = -1.1, col = "grey", cex = 6, fontface = "bold", 
            alpha = 0.8)
    }
    pl <- ggplot(vvv__1, do.call(aes_string, arg_aes_string)) + vvv__20 + vvv__16 + vvv__19 + vvv__15 + vvv__17 + vvv__18
    print(pl)
    if (!is.null(vvv__6) & !is.null(vvv__7) & !is.null(vvv__8)) {
        fn_string <- paste(vvv__8, vvv__7, sep = ".")
        ggsave(fn_string, plot = pl, device = vvv__7, path = vvv__6)
    }
    return(list(data = vvv__1, modelv = pl))
}
