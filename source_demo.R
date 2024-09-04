default_columns <- c("Ozone", "Solar.R", "Wind", "Temp")


get_summary <- function(df, columns = default_columns) {
    smmry <- summary(df[columns])
    return(smmry)
}

factor_conversion <- function(airq) {
    airq$Month_f = factor(airq$Month, levels = 5:9, labels = month.abb[5:9])
    return(airq)
}

plot_airquality <- function(airq) {
    ggplot(data = airq, mapping = aes(x = Solar.R, y = Ozone)) +
        geom_smooth(method = "lm", formula = y ~ x) +
        geom_point(aes(color = Month_f), alpha = 0.7) + 
        facet_wrap(. ~ Month_f) +
        ylab("Ozone (ppb)") +
        xlab("Solar radiation (lang)") +
        theme(legend.title = element_blank())
}



# mtcars %>% 
#     group_by(vs, gear) %>% 
#     summarize(count = n(), .groups = "drop") %>%
#     group_by(vs) %>%
#     mutate(perc = count / sum(count) * 100)



