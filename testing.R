load('testdata/ISIP_raw.Rdata')

excludedSubjects <- c(1, 902, 903, 904, 910, 913, 914, 999)


simon2 <- simon
previous_type <- precedent(simon$values.congruence)
simon2$previous_type <- previous_type
simon2 <- filter(simon2, !(trialnum == 1))

td <- simon2 %>%
  filter(blockcode == "testblock") %>%
  select(subject, values.congruence, previous_type, latency, correct) %>%
  group_by(subject, values.congruence, previous_type) %>%
  summarize(mean_latency = mean(latency),
            accuracy = mean(correct),
            sd_lat = sd(latency),
            n = n()) %>%
  unite(temp, mean_latency, accuracy, sd_lat, n, sep = "W", remove = TRUE)  %>%
  unite(temptype, values.congruence, previous_type) %>%
  spread(temptype, temp) %>%
  separate(congruent_congruent, c("CC_latency", "CC_accuracy", "CC_sd", "CC_n"), sep = "W", convert = TRUE) %>%
  separate(incongruent_congruent, c("IC_latency", "IC_accuracy", "IC_sd", "IC_n"), sep = "W", convert = TRUE) %>%
  separate(congruent_incongruent, c("CI_latency", "CI", "CI_sd", "CI_n"), sep = "W", convert = TRUE) %>%
  separate(incongruent_incongruent, c("II_latency", "II_accuracy", "II_sd", "II_n"), sep = "W", convert = TRUE) %>%
  mutate(comp_t_diff = as.numeric(IC_latency) - as.numeric(CC_latency),
         inc_t_diff = as.numeric(II_latency) - as.numeric(CI_latency))

mutate(d = t_diff / sqrt( (cong_sd^2*(cong_n - 1) + inc_sd^2*(inc_n - 1) / (cong_n + inc_n + 1))))

