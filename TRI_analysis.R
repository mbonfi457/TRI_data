library(tidyverse)

# Summary of Toxics Release Inventory (TRI)
df_2021 <- read.csv("2021_us.csv") 
glimpse(df_2021)
names(df_2021)

# Fix column names
colnames(df_2021) <- gsub("^X\\d+\\.+", "", colnames(df_2021))


facility_count <- df_2021 %>% 
  count(FACILITY.NAME, sort = T)


facility_count_short <- facility_count %>% 
  filter(n >= 50)


# graph top toxics releasers in descending order (>= 50)
facility_count_short %>% 
  ggplot(aes(x=reorder(FACILITY.NAME, n), y=n, fill="#53868b"))+
  geom_bar(stat='identity')+
  theme(legend.position = "none", axis.text = element_text(hjust=1, size=8))+
  scale_fill_manual(values = "#53868b")+
  labs(title = "Frequency of Releases by Facility (top 50)",
       x= "Facility",
       y="Count")+
  scale_y_discrete(expand = c(0, 0))+
  coord_flip()

# Determine the frequency of Carcinogenic vs. Non-carcinogenic releases
num_carcinogenic <- df_2021 %>% 
  count(CARCINOGEN)
num_carcinogenic %>% 
  ggplot(aes(x=CARCINOGEN, y=n, fill=CARCINOGEN))+
  geom_bar(stat='identity', width = 0.4)+
  scale_fill_hue(c=40)+
  labs(title = "Carcinogenic vs. Non-carcinogenic Chemicals Released",
       y="Count")

# Plot carinogen vs. non-carcinogen for the top 3 toxics releasers
top_3_count <- facility_count[1:10,]
top_3 <- top_3_count$FACILITY.NAME

top_3_df <- df_2021 %>% 
  filter(FACILITY.NAME %in% as.list(top_3))

top_3_plot <- top_3_df %>% 
  select(all_of(c('FACILITY.NAME','CARCINOGEN'))) %>% 
  ggplot(aes(x=FACILITY.NAME, fill=CARCINOGEN))+
  geom_bar(position = 'dodge')+
  scale_fill_hue(c=40)+
  theme(axis.text=element_text(hjust = 1))+
  labs(title = "Releases from Top 10 Releasers by Carcinogenic Status",
      x = "Facility Name",
      y = "Count")+
  scale_y_discrete(expand = c(0, 0))+
  coord_flip()
top_3_plot

# Determine most common type of release
release_types <- sort(table(df_2021$CHEMICAL), decreasing = TRUE)
top_10_chems <- as.data.frame(release_types[1:10])
top_25_chems <- as.data.frame(release_types[1:25])
top_50_chems <- as.data.frame(release_types[1:50])

top_25_chems %>% 
  ggplot(aes(x=reorder(Var1,Freq), y=Freq, fill = "#53868b"))+
  geom_bar(stat='identity')+
  labs(title = "Frequency of Chemicals Released",x="Chemical", y="Count")+
  theme(axis.text=element_text(hjust=1), legend.position = "none")+
  scale_fill_manual(values = "#53868b")+
  scale_y_discrete(expand = c(0, 0))+
  coord_flip()

# Determine frequency of releases by state
state_frequency <- as.data.frame(sort(table(df_2021$ST), decreasing = TRUE))
state_frequency %>% 
  ggplot(aes(x=Var1, y=Freq, fill = "#53868b"))+
  geom_bar(stat='identity')+
  theme(legend.position = "none")+
  labs(title = "Frequency of Release by State", x="State", y="Count")+
  scale_fill_manual(values="#53868b")+
  scale_y_discrete(expand = c(0,0))


# Determine biggest releasers in Tennessee
TN_releasers <- as.data.frame(select(df_2021, c('ST', 'FACILITY.NAME'))) %>% 
  filter(ST == "TN")

TN_releasers <- as.data.frame(sort(table(TN_releasers$FACILITY.NAME), decreasing = TRUE))
TN_releasers[1:20,] %>% 
  ggplot(aes(x=reorder(Var1, Freq), y=Freq, fill = "53868b"))+
  geom_bar(stat='identity')+
  theme(legend.position = "none")+
  labs(title = "Highest Releasers in Tennessee", x="Facility", y="Count")+
  theme(axis.text=element_text(hjust=1), legend.position = "none")+
  scale_fill_manual(values="#53868b")+
  scale_y_discrete(expand = c(0,0))+
  coord_flip()
