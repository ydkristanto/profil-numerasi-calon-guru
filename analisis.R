library(tidyverse)
library(readxl)
library(ggrepel)

# Tren PISA Indonesia
data_tren_pisa <- read_excel("data/tren_PISA.xlsx", 
                        na = "m")
tren_pisa_id <- data_tren_pisa %>% 
  filter(
    negara %in% c("Indonesia", "OECD average-23")
  ) %>% 
  select(
    !contains("se_")
  ) %>% 
  pivot_longer(
    cols = contains("skor_"),
    names_to = "tahun",
    values_to = "skor"
  ) %>% 
  mutate(
    tahun = as.integer(str_remove(tahun, "skor_"))
  )
tren_pisa_idx <- tren_pisa_id %>% 
  filter(negara == "Indonesia") %>% 
  select(literasi, negara, tahun, skor) %>% 
  mutate(skor = round(skor))
tren_pisa_id %>% 
  mutate(
    negara = ifelse(
      negara == "OECD average-23",
      "Rerata OECD (23 negara)",
      negara
    )
  ) %>% 
  ggplot(aes(x = tahun, y = skor, colour = negara)) + 
  geom_line(aes(group = negara)) + 
  geom_point(size = 1) + 
  geom_line(
    data = tren_pisa_idx,
    stat = "smooth",
    method = "lm",
    formula = y ~ poly(x, degree = 2),
    color = "blue",
    alpha = .6
  ) + 
  geom_text(
    data = tren_pisa_idx,
    aes(x = tahun, y = skor, label = skor,fontface = "bold"),
    nudge_y = 8,
    size = 2
  ) + 
  scale_color_viridis_d(
    name = ""
  ) + 
  facet_wrap(facets = vars(literasi), scales = "free_x") + 
  theme_minimal(base_size = 9) + 
  theme(
    legend.position = "top"
  ) + 
  labs(
    x = "Tahun",
    y = "Skor"
  )
ggsave(
  filename = "plot/tren_pisa_id.png",
  plot = last_plot(),
  width = 12.8,
  height = 7.2,
  units = "cm",
  dpi = 300,
  bg = "white"
)


