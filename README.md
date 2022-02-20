# make_your_graphs_badass
my_packages <- c("broom", "ggrepel", "coefplot", "cowplot", "survival", "gapminder", "ggrepel", "ggridges", "gridExtra", "here", "interplot", "margins", "maps", "mapproj", "mapdata", "MASS", "quantreg", "rlang", "scales", "survey", "srvyr", "viridis", "viridislite", "devtools")

install.packages(my_packages, repos = "http://cran.rstudio.com")

library(tidyverse)
library(ggplot2)
library(socviz)
library(gapminder)

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

gapminder

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))

p + geom_point()


p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point() + geom_smooth()


p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))

p + geom_point() + geom_smooth(method = "gam") + scale_x_log10()


p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point() + geom_smooth(formula = y ~ x, methodmethod = "gam") + scale_x_log10(labels = scales::dollar)


p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill(continent_colors)))
p + geom_point() + geom_smooth(formula = y ~ x, method = "loess") + scale_x_log10()


p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(color = "purple") + geom_smooth(formula = y ~ x, method = "loess") + scale_x_log10()

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(alpha = 0.3) + geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + scale_x_log10() + labs(x = "GDP Per Capita", y = "Life Expectancy in Years", title = "Economic Growth and Life Expectancy", subtitle = "Data points are country-years", caption = "Source::Gapminder.")

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = "continent", fill = "continent"))
p + geom_point() + geom_smooth(formula = y ~ x, method = "loess") + scale_x_log10()

#############################################

### Aesthetics ###

############################################

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = "continent")) + geom_smooth(formula = y ~ x, method = "loess") + scale_x_log10()


#############################################

### Mapping a continous variable to color ###

############################################

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = log(pop))) + scale_x_log10()

#############################################

### Cluster with a regression line ###

############################################

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(alpha = 0.3) + geom_smooth(formula = y ~ x, method = "gam") + scale_x_log10(labels = scales::dollar) + labs(x = "GDP Per Capita", y = "Life Expectancy in Years", title = "Economic Growth and Life Expectancy", subtitle = "Data Points are country-years", caption = "Source: Gapminder.")


#############################################

### Mapping contients with color ###

############################################

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent))
p + geom_point() + geom_smooth(formula = y ~ x, method = "loess") + scale_x_log10()

#############################################

### Mapping contients with color + colored curves ###

############################################


p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))
p + geom_point() + geom_smooth(formula = y ~ x, method = "loess") + scale_x_log10()


#############################################

### Row of Graphics with 2 variables ###

############################################

p <- ggplot(data = gapminder, mapping = aes(x = year, y = gdpPercap))
p + geom_line(aes(group = country)) + geom_smooth(size = 1.1, method = "loess", se = FALSE) + scale_x_log10(labels=scales::dollar) + facet_wrap(~ continent, ncol = 5) + labs(x = "Year", y = "GDP per Capita", title = "GDP per capita on Five Continents") 

#############################################

### Row of Stacked Graphics with 2 variables ###

############################################
p <- ggplot(data = gss_sm, mapping = aes(x = age, y = childs))
p + geom_point(alpha = 0.2) + geom_smooth() + facet_grid(sex ~ race)

#############################################

### Colored Density Distributions ###

############################################
p <- ggplot(data = midwest, mapping = aes(x = area, fill = state, color = state))
p + geom_density(alpha = 0.3)


oh_wi <- c("OH", "WI")
p <- ggplot(data = subset(midwest, subset = state %in% oh_wi), mapping = aes(x = area, fill = state, color = state))
p + geom_density(alpha = 0.3, mapping = (aes(y = ..scaled..)))


#############################################

### Postitve and Negative Bar Graph ###

############################################

oecd_sum

p <- ggplot(data = oecd_sum, mapping = aes(x = year, y = diff, fill = hi_lo))
p + geom_col() + guides(fill = FALSE) + labs(x = NULL, y = "Difference in Years", title = "The US Life Expectance Gap", subtitle = "Difference between US and OECD average life expectancies, 1960-2015", caption = "Data: OECD. After a chart by Diego Macall, RT America, January 27th 2100")

#############################################

### Graph Tables, Add Labels Make Notes ###

############################################

rel_by_region <- gss_sm %>% group_by(bigregion, religion) %>% summarize(N = n()) %>% mutate(freq = N / sum(N), pct = round((freq*100), 0))

rel_by_region %>% group_by(bigregion) %>% summarize(total = sum(pct))

p <- ggplot(rel_by_region, aes(x = bigregion, y = pct, fill = religion))
p + geom_col(position = "dodge2") + labs(x = "Region", y = " Percent", fill = "Religion") + theme(legend.position = "top")

p <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion))
p + geom_col(position = "dodge2") + labs(x = NULL, y = " Percent", fill = "Religion") + guides(fill = FALSE) + coord_flip() + facet_grid(~ bigregion)


#############################################

### Continous Variables by Group or Category ###

############################################

organdata %>% select(1:6) %>% sample_n(size = 10)

p <- ggplot(data = organdata,mapping = aes(x = year, y = donors))
p + geom_point

### Multi-Graph ###

p <- ggplot(data = organdata, mapping = aes(x = year, y = donors))
p + geom_line(aes(group = country)) + facet_wrap(~country)

### Box Plot ###

p <- ggplot(data = organdata, mapping = aes(x = country, y = donors))
p + geom_boxplot()

### Horizontal Box Plot ###

p <- ggplot(data = organdata, mapping = aes(x = country, y = donors))
p + geom_boxplot() + coord_flip()

### Hierarchy Box Plot ###

p <- ggplot(data = organdata, mapping = aes(x = reorder(country, donors, na.rm = TRUE), y = donors))
p + geom_boxplot() + labs(x = NULL) + coord_flip()

### Hierarchy Box Plot Colors ###

p <- ggplot(data = organdata, mapping = aes(x = reorder(country, donors, na.rm = TRUE), y = donors, fill = world))
p + geom_boxplot() + labs(x = NULL) + coord_flip() + theme(legend.position = "top")

### Colored Dots ###

p <- ggplot(data = organdata, mapping = aes(x = reorder(country, donors, na.rm = TRUE), y = donors, color = world))
p + geom_point() + labs(x = NULL) + coord_flip() + theme(legend.position = "top")

### Thick Colored Dots###

by_country <- organdata %>% group_by(consent_law, country) %>% summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>% ungroup()
p <- ggplot(data = by_country, mapping = aes(x = donors_mean, y = reorder(country, donors_mean), color = consent_law))
p + geom_point(size = 3) + labs(x = "Donor Procurement Rate", y = "", color = "Consent Law") + theme(legend.position = "top")

### Stacked Black Dots###

by_country <- organdata %>% group_by(consent_law, country) %>% summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>% ungroup()
p <- ggplot(data = by_country, mapping = aes(x = donors_mean, y = reorder(country, donors_mean)))
p + geom_point(size = 3) + facet_wrap(~ consent_law, scales = "free_y", ncol = 1) + labs(x = "Donor Procurement Rate", y = "")

p <- ggplot(data = by_country, mapping = aes(x = country, donors_mean, y = donors_mean))
p + geom_pointrange(mapping = aes(ymin = donors_mean - donors_sd, ymax = donors_mean + donors_sd)) + labs(x = "", y = "Donor Procurement Rate") + coord_flip()

### Dots with Names ###
p <- ggplot(data = by_country, mapping = aes(x = roads_mean, y = donors_mean))
p + geom_point() + geom_text(mapping = aes(label = country))

### Dots with Correctly Placed Names ###
p <- ggplot(data = by_country, mapping = aes(x = roads_mean, y = donors_mean))
p + geom_point() + geom_text(mapping = aes(label = country), hjust = 0)

### Quadrant ###
library (ggrepel)
elections_historic %>% select(2:7)

p_title <- "Presidential Elections: Popular & Electoral College Margins"
p_subtitle <- "1824-2016"
p_caption <- "Data for 2016 are provisional."
x_label <- "Winner´s share of Popular Vote"
y_label <- "Winner´s share of Electoral College Votes"

p <- ggplot(elections_historic, aes(x = popular_pct, y = ec_pct, label = winner_label)) 
p + geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") + geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") + geom_point() + geom_text_repel() + scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + labs(x = x_label, y_label, title = p_title, subtitle = p_subtitle, caption = p_caption)

### Quadrant ###
library(survival)
library(broom)

out_cph <- coxph(Surv(time, status) ~ age + sex, data = lung)
out_surv <- survfit(out_cph)
out_tidy <- tidy(out_survey)

p <- ggplot(data = out_tidy, mapping = aes(x = time,estimate))
p + geom_line() + geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high), alpha = 0.2)

#############################################

###################### Maps ###

############################################

library(GGally)

organdata_sm <- organdata %>% select(donors, pop_dens, pubhealth, roads, consent_law)

ggpairs(data = organdata_sm, mapping = aes(color = consent_law),
        upper = list(continuous = wrap("density"), combo = "box_no_facet"),
        lower = list(continuous = wrap("points"), combo = wrap("dot_no_facet"))) 

election %>% select(state, total_vote, r_points, pct_trump, party, census) %>%
  sample_n(5)

############# Background to Maps Building ##########

party_colors <- c("#2E74C0", "#CB454A")

p0 <- ggplot(data = subset(election, st %nin% "DC"),
             mapping = aes(x = r_points, y = reorder(state, r_points), color = party))

p1 <- p0 + geom_vline(xintercept = 0, color = "gray30") + geom_point(size = 2)

p2 <- p1 + scale_color_manual(values = party_colors)

p3 <- p2 + scale_color_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30, 40),
                                  labels = c("30\n (Clinton)", "20", "10", "0",
                                             "10", "20", "30", "40\n(Trump)"))

p3 + facet_wrap(~ census, ncol = 1, sclaes = "free_y") +
  guides(color=FALSE) + labs(x = "Point Margin", y = "") + 
  theme(axis.text=element_text(size = 8))

############# Actual Map #############

library(maps)
us_states <- map_data("state")
head(us_states)

dim(us_states)

############# Black and White Map #############
p <- ggplot(data = us_states, mapping = aes(x = long, y = lat, group = group))

p + geom_polygon(fill = "white", color = "black")


############# Black and White Map #############

p <- ggplot(data = us_states, mapping = aes(x = long, y = lat, group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)

############# Color White Map #############

p <- ggplot(data = us_states, mapping = aes(x = long, y = lat, group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)

############# Curved Latitude and Longitude Map #############

p <- ggplot(data = us_states, mapping = aes(x = long, y = lat, group = group, fill = region))
p + geom_polygon(color = "gray90", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45) + guides(fill = FALSE)

############# Two Category Map #############

election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election)

p <- ggplot(data = us_states_elec, aes(x = long, y = lat, group = group, fill = party))
p + geom_polygon(color = "gray90", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45)

############# Gradient Two Category Map #############

p0 <- ggplot(data = us_states_elec, mapping = aes(x = long, y = lat, group = group, fill = party))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p2 <- p1 + scale_fill_manual(values = party_colors) + labs(title = "Election Results 2016", fill = NULL)

p2 <- theme_map()
############# Solid Color Two Category Map Version 2 #############

p0 <- ggplot(data = us_states_elec, mapping = aes(x = long, y = lat, group = group, fill = party))

p1 <- p0 + geom_polygon(color = "gray30", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p2 <- p1 + scale_fill_manual(values = party_colors) + labs(title = "Election Results 2016", fill = NULL)

p2 + theme()

############# Gradient Blue + Red Two Category Map Version 2  #############

p0 <- ggplot(data = us_states_elec, mapping = aes(x = long, y = lat, group = group, fill = pct_trump))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p1 + labs(title = "Trump vote", fill = NULL) + theme() + labs(fill = "Percent")
############# Gradient Blue + Red Two Category Map Version 2  #############

p2 <- p1 + scale_fill_gradient(low = "White", high = "#CB454A") + labs(title = "Trumo vote")

p2 + theme() + labs(fill = "Percent")

############# Pale Gradient Blue + Red Two Category Map Version 2  #############

p0 <- ggplot(data = subset(us_states_elec, region %nin% "district of columbia"), aes(x = long, y = lat, group = group, fill = d_points))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p2 <- p1 + scale_fill_gradient2(low = "red", mid = scales::muted("purple"), high = "blue") + labs(title = "blue") + labs(title = "Winning margins")
                                                                                              
p2 + theme() + labs(fill = "Percent")

#############  Country Level Gradient color Map  #############
county_map %>% sample_n(5)

county_data %>% select(id, name, state, pop_dens, pct_black) %>% sample_n(5)

county_full <- left_join(county_map, county_data, by = "id")

p <- ggplot(data = county_full, mapping = aes(x = long, y = lat, group = group, fill = pop_dens))

p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

p2 <- p1 + scale_fill_brewer(palette = "Blues", labels = c("0-10", "10-50", "50-100", "100-500", "500-1,000", "1,000-5,000", ">5,000")) 

p2 + theme() + labs(fill = "Percent")
