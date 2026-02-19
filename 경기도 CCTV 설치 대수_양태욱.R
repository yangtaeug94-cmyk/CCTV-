# 1. 패키지 및 설정 (기존 동일)
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
sf_use_s2(FALSE)

# 2. 데이터 로드
setwd("C:/Users/KH/Downloads/cctv") 
map <- st_read('gadm41_KOR_2.json')
# 혹시 모르니 행 전체를 문자로 읽어서 검색 누락 방지
cctv_data <- read.csv("CCTV정보.csv", fileEncoding = "CP949", stringsAsFactors = FALSE)

# 3. 경기도 지도 추출
gyeonggi_map <- map %>% filter(NAME_1 == "Gyeonggi-do")

# 4. 경기도 데이터 집계 - 주소지 최우선 로직
gyeonggi_cctv <- cctv_data %>%
  # 한 줄 통째로 합치기 (데이터 밀림 현상 방어)
  mutate(all_text = apply(., 1, paste, collapse = " ")) %>%
  # 경기도권 데이터 필터링
  filter(grepl("경기|김포|화성|평택|여주|안산|수원", all_text)) %>% 
  
  # 관리기관이 어디든 주소에 해당 지역명이 있으면 그 지역으로!
  mutate(location = case_when(
    str_detect(all_text, "화성시") ~ "화성시",
    str_detect(all_text, "김포시") ~ "김포시",
    str_detect(all_text, "평택시") ~ "평택시",
    str_detect(all_text, "여주시") ~ "여주시",
    str_detect(all_text, "안산시") ~ "안산시",
    str_detect(all_text, "수원시") ~ "수원시",
    TRUE ~ str_extract(all_text, "(안성시|안양시|부천시|동두천시|가평군|고양시|군포시|구리시|과천시|광주시|광명시|하남시|이천시|남양주시|오산시|파주시|포천시|성남시|시흥시|의정부시|의왕시|양주시|양평군|연천군|용인시)")
  )) %>%
  
  # [숫자 추출] 콤마 사이의 단독 숫자를 찾아 카메라 대수로 인식
  # 화성시 예시의 '5' 또는 '1' 같은 숫자를 정밀하게 타격
  mutate(count_val = str_extract(all_text, ",(\\d+),")) %>% 
  mutate(count_val = as.numeric(str_replace_all(count_val, ",", ""))) %>%
  # 숫자가 비정상적(NA나 1000대 이상)이면 최소 1대로 간주
  mutate(count_val = ifelse(is.na(count_val) | count_val > 1000, 1, count_val)) %>%
  
  group_by(location) %>%
  summarise(total_count = sum(count_val, na.rm = TRUE)) %>%
  filter(!is.na(location))

# 5. 매칭 테이블 (기존 동일)
name_map_gg <- tibble::tribble(
  ~NAME_2, ~location,
  "Ansan", "안산시", "Anseong", "안성시", "Anyang", "안양시",
  "Bucheon", "부천시", "Dongducheon", "동두천시", "Gapyeong", "가평군",
  "Gimpo", "김포시", "Goyang", "고양시", "Gunpo", "군포시",
  "Guri", "구리시", "Gwacheon", "과천시", "Gwangju", "광주시",
  "Gwangmyeong", "광명시", "Hanam", "하남시", "Hwaseong", "화성시",
  "Icheon", "이천시", "Namyangju", "남양주시", "Osan", "오산시",
  "Paju", "파주시", "Pocheon", "포천시", "Pyeongtaek", "평택시",
  "Seongnam", "성남시", "Siheung", "시흥시", "Suwon", "수원시",
  "Uijeongbu", "의정부시", "Uiwang", "의왕시", "Yangju", "양주시",
  "Yangpyeong", "양평군", "Yeoju", "여주시", "Yeoncheon", "연천군",
  "Yongin", "용인시"
)

# 6. 결합
gyeonggi_final <- gyeonggi_map %>%
  left_join(name_map_gg, by = "NAME_2") %>%
  left_join(gyeonggi_cctv, by = "location")

# 7. 최종 경기도 시각화 (특정 지역 글자색 흰색 보정)
ggplot(gyeonggi_final) +
  geom_sf(aes(fill = total_count), color = "white", size = 0.3) +
  geom_sf_text(
    aes(
      label = location,
      # 지정하신 4개 지역은 무조건 흰색(white), 나머지는 검은색(black)
      color = ifelse(location %in% c("수원시", "용인시", "이천시", "남양주시"), "white", "black")
    ),
    size = 2.8,
    fontface = "bold"
  ) +
  scale_color_identity() + 
  scale_fill_gradientn(
    colors = c("#F2E6FF", "#D1B2FF", "#A374FF", "#6E31FF", "#3A00B5"), 
    name = "CCTV 대수",
    labels = scales::comma,
    na.value = "#FFCCCC" 
  ) +
  labs(
    title = "경기도 시·군별 CCTV 설치 현황",
    caption = "데이터 출처: 공공데이터포털"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(), 
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = 16, color = "#3A00B5"),
    
    # 범례 크기 조절
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm")
  )