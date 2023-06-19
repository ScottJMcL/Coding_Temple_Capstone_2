
################################################################
######################## Changed direction #####################
####################### First take at bottom ###################
################################################################


top_and_bottom_CAV_states <-  list(
  'CA', 'WA','OR','HI','VT','MA','MD','CO','NV','VA', 'AZ',
  'MI', 'ND','LA','WY','AL','SD','WV','AR','OK','KY'
)

nei_states <- filter(nei_by_pollutant,State %in% top_and_bottom_CAV_states)

# View(nei_states)

avg_em_by_state <- nei_by_pollutant %>%
  group_by(Pollutant, CAV_adoption,Year) %>%
  summarise(avg_em = mean(Emmisions_per_100k))

View(avg_em_by_state)

######################## Greenhouse Gasses ##################### 
######################## Carbon Dioxide ######################## 

CO2_em <- filter(nei_states, Pollutant == "Carbon Dioxide")
# colnames(CO2_em)
# View(CO2_em)

ggplot(data = CO2_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Carbon Dioxide Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Carbon Monoxide ######################## 

CO_em <- filter(nei_states, Pollutant == "Carbon Monoxide")

# View(CO_em)

ggplot(data = CO_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Carbon Monoxide Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Methane ######################## 

CH4_em <- filter(nei_states, Pollutant == "Methane")



ggplot(data = CH4_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Methane Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))


######################## Nitrous Oxide ######################## 

NOx_em <- filter(nei_states, Pollutant == "Nitrous Oxide")

ggplot(data = NOx_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Nitrous Oxide Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Health Hazards ######################## 
######################## Chromium (VI) ######################## 

Cr_VI_em <- filter(nei_states, Pollutant == "Chromium (VI)")

ggplot(data = Cr_VI_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Chromium (VI) Emisions",
    x = "Year",
    y = "Anual Lb's of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Manganese ######################## 

Mn_em <- filter(nei_states, Pollutant == "Manganese")

ggplot(data = Mn_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Manganese Emisions",
    x = "Year",
    y = "Anual Lb's of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Mercury ######################## 

Hg_em <- filter(nei_states, Pollutant == "Mercury")

ggplot(data = Hg_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Mercury Emisions",
    x = "Year",
    y = "Anual Lb's of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Nickel ######################## 

Ni_em <- filter(nei_states, Pollutant == "Nickel")

ggplot(data = Ni_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Nickel Emisions",
    x = "Year",
    y = "Anual Lb's of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))


######################## Nitrogen Oxides ######################## 

NOx_em <- filter(nei_states, Pollutant == "Nitrogen Oxides")

ggplot(data = NOx_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Nitrogen Oxides Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## PM10 Primary (Filt + Cond) ######################## 

PM10_em <- filter(nei_states, Pollutant == "PM10 Primary (Filt + Cond)")

ggplot(data = PM10_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Large Particulate Matter (<10um but >2.5um) Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## PM2.5 Primary (Filt + Cond) ######################## 

PM25_em <- filter(nei_states, Pollutant == "PM2.5 Primary (Filt + Cond)")

ggplot(data = PM25_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Fine Particulate Matter (<2.5um) Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Sulfur Dioxide ######################## 

SO2_em <- filter(nei_states, Pollutant == "Sulfur Dioxide")

ggplot(data = SO2_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Sulfur Dioxide Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Volatile Organic Compounds ######################## 

VOC_em <- filter(nei_states, Pollutant == "Volatile Organic Compounds")

ggplot(data = VOC_em, mapping = aes(x = Year, y= Emmisions_per_100k)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_smooth(aes(color = CAV_adoption)) +
  labs(
    title = "Volatile Organic Compounds (VOC) Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))


################################################################
################################################################
########################### Averages ###########################
################################################################
################################################################

######################## Greenhouse Gasses ##################### 
######################## Carbon Dioxide ######################## 

CO2_em <- filter(avg_em_by_state, Pollutant == "Carbon Dioxide")
# colnames(CO2_em)
# View(CO2_em)

ggplot(data = CO2_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Carbon Dioxide Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Carbon Monoxide ######################## 

CO_em <- filter(avg_em_by_state, Pollutant == "Carbon Monoxide")

# View(CO_em)

ggplot(data = CO_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Carbon Monoxide Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Methane ######################## 

CH4_em <- filter(avg_em_by_state, Pollutant == "Methane")



ggplot(data = CH4_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Methane Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))


######################## Nitrous Oxide ######################## 

NOx_em <- filter(avg_em_by_state, Pollutant == "Nitrous Oxide")

ggplot(data = NOx_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Nitrous Oxide Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Health Hazards ######################## 
######################## Chromium (VI) ######################## 

Cr_VI_em <- filter(avg_em_by_state, Pollutant == "Chromium (VI)")

ggplot(data = Cr_VI_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Chromium (VI) Emisions",
    x = "Year",
    y = "Anual Lb's of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Manganese ######################## 

Mn_em <- filter(avg_em_by_state, Pollutant == "Manganese")

ggplot(data = Mn_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Manganese Emisions",
    x = "Year",
    y = "Anual Lb's of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Mercury ######################## 

Hg_em <- filter(avg_em_by_state, Pollutant == "Mercury")

ggplot(data = Hg_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Mercury Emisions",
    x = "Year",
    y = "Anual Lb's of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Nickel ######################## 

Ni_em <- filter(avg_em_by_state, Pollutant == "Nickel")

ggplot(data = Ni_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Nickel Emisions",
    x = "Year",
    y = "Anual Lb's of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))


######################## Nitrogen Oxides ######################## 

NOx_em <- filter(avg_em_by_state, Pollutant == "Nitrogen Oxides")

ggplot(data = NOx_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Nitrogen Oxides Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## PM10 Primary (Filt + Cond) ######################## 

PM10_em <- filter(avg_em_by_state, Pollutant == "PM10 Primary (Filt + Cond)")

ggplot(data = PM10_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Large Particulate Matter (<10um but >2.5um) Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## PM2.5 Primary (Filt + Cond) ######################## 

PM25_em <- filter(avg_em_by_state, Pollutant == "PM2.5 Primary (Filt + Cond)")

ggplot(data = PM25_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Fine Particulate Matter (<2.5um) Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Sulfur Dioxide ######################## 

SO2_em <- filter(avg_em_by_state, Pollutant == "Sulfur Dioxide")

ggplot(data = SO2_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Sulfur Dioxide Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))

######################## Volatile Organic Compounds ######################## 

VOC_em <- filter(avg_em_by_state, Pollutant == "Volatile Organic Compounds")

ggplot(data = VOC_em, mapping = aes(x = Year, y= avg_em)) +
  geom_point(aes(color = CAV_adoption)) +
  geom_line(aes(color = CAV_adoption)) +
  labs(
    title = "Volatile Organic Compounds (VOC) Emisions",
    x = "Year",
    y = "Anual tons of emisions"
  ) +
  scale_x_continuous(breaks = 2008:2020) +
  theme(plot.title = element_text(hjust = 0.5))




# ################################################################
# ######################## Changed direction #####################
# ################## Cut & paste unused work to bottom  ##########
# ################################################################

# 
# avg_em_by_state <- nei_by_pollutant %>%
#   group_by(Pollutant, CAV_adoption,Year) %>%
#   summarise(avg_em = mean(Emmisions_per_100k))
# 
# View(avg_em_by_state)
# 
# ################################################################
# ######################## Changed direction #####################
# ########################   Skip to bottom  #####################
# ################################################################
# 
# 
# ######################## Greenhouse Gasses ##################### 
# ######################## Carbon Dioxide ######################## 
# 
# CO2_em <- filter(avg_em_by_state, Pollutant == "Carbon Dioxide")
# 
# # View(CO2_em)
# 
# ggplot(data = CO2_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Carbon Dioxide Emisions",
#     x = "Year",
#     y = "Anual tons of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ######################## Carbon Monoxide ######################## 
# 
# CO_em <- filter(avg_em_by_state, Pollutant == "Carbon Monoxide")
# 
# # View(CO_em)
# 
# ggplot(data = CO_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Carbon Monoxide Emisions",
#     x = "Year",
#     y = "Anual tons of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ######################## Methane ######################## 
# 
# CH4_em <- filter(avg_em_by_state, Pollutant == "Methane")
# 
# 
# 
# ggplot(data = CH4_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Methane Emisions",
#     x = "Year",
#     y = "Anual tons of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# ######################## Nitrous Oxide ######################## 
# 
# NOx_em <- filter(avg_em_by_state, Pollutant == "Nitrous Oxide")
# 
# ggplot(data = NOx_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Nitrous Oxide Emisions",
#     x = "Year",
#     y = "Anual tons of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ######################## Health Hazards ######################## 
# ######################## Chromium (VI) ######################## 
# 
# Cr_VI_em <- filter(avg_em_by_state, Pollutant == "Chromium (VI)")
# 
# ggplot(data = Cr_VI_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Chromium (VI) Emisions",
#     x = "Year",
#     y = "Anual Lb's of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ######################## Manganese ######################## 
# 
# Mn_em <- filter(avg_em_by_state, Pollutant == "Manganese")
# 
# ggplot(data = Mn_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Manganese Emisions",
#     x = "Year",
#     y = "Anual Lb's of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ######################## Mercury ######################## 
# 
# Hg_em <- filter(avg_em_by_state, Pollutant == "Mercury")
# 
# ggplot(data = Hg_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Mercury Emisions",
#     x = "Year",
#     y = "Anual Lb's of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ######################## Nickel ######################## 
# 
# Ni_em <- filter(avg_em_by_state, Pollutant == "Nickel")
# 
# ggplot(data = Ni_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Nickel Emisions",
#     x = "Year",
#     y = "Anual Lb's of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# ######################## Nitrogen Oxides ######################## 
# 
# NOx_em <- filter(avg_em_by_state, Pollutant == "Nitrogen Oxides")
# 
# ggplot(data = NOx_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Nitrogen Oxides Emisions",
#     x = "Year",
#     y = "Anual tons of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ######################## PM10 Primary (Filt + Cond) ######################## 
# 
# PM10_em <- filter(avg_em_by_state, Pollutant == "PM10 Primary (Filt + Cond)")
# 
# ggplot(data = PM10_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Large Particulate Matter (<10um but >2.5um) Emisions",
#     x = "Year",
#     y = "Anual tons of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ######################## PM2.5 Primary (Filt + Cond) ######################## 
# 
# PM25_em <- filter(avg_em_by_state, Pollutant == "PM2.5 Primary (Filt + Cond)")
# 
# ggplot(data = PM25_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Fine Particulate Matter (<2.5um) Emisions",
#     x = "Year",
#     y = "Anual tons of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ######################## Sulfur Dioxide ######################## 
# 
# SO2_em <- filter(avg_em_by_state, Pollutant == "Sulfur Dioxide")
# 
# ggplot(data = SO2_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Sulfur Dioxide Emisions",
#     x = "Year",
#     y = "Anual tons of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ######################## Volatile Organic Compounds ######################## 
# 
# VOC_em <- filter(avg_em_by_state, Pollutant == "Volatile Organic Compounds")
# 
# ggplot(data = VOC_em, mapping = aes(x = Year, y= avg_em)) +
#   geom_line(aes(color = CAV_adoption)) +
#   geom_point(aes(color = CAV_adoption)) +
#   labs(
#     title = "Volatile Organic Compounds (VOC) Emisions",
#     x = "Year",
#     y = "Anual tons of emisions"
#   ) +
#   scale_x_continuous(breaks = 2008:2020) +
#   theme(plot.title = element_text(hjust = 0.5))
