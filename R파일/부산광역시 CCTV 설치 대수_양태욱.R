# 1. 필수 패키지 로드 및 기강 잡기
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

sf_use_s2(FALSE) # 경고 메시지 차단

# 2. 경로 설정 및 데이터 불러오기
setwd("C:/Users/KH/Downloads/cctv") 

map <- st_read('gadm41_KOR_2.json')
cctv_data <- read.csv("CCTV정보.csv", fileEncoding = "CP949")

# 3. 지도에서 부산광역시만 추출
busan_map <- map %>% filter(NAME_1 == "Busan")

# 4. CCTV 데이터 부산 구별 집계
busan_cctv <- cctv_data %>%
  mutate(hint = paste0(관리기관명, 소재지도로명주소)) %>%
  filter(grepl("부산|기장|강서구", hint)) %>% 
  mutate(location = str_extract(hint, "[가-힣]+(구|군)")) %>%
  group_by(location) %>%
  summarise(total_count = sum(카메라대수, na.rm = TRUE)) %>%
  filter(!is.na(location))

# 5. 부산 영문-한글 매칭 (기존 로직 유지)
name_map_busan <- tibble::tribble(
  ~NAME_2, ~location,
  "Buk", "북구", "Busanjin", "부산진구", "Dong", "동구",
  "Dongnae", "동래구", "Gangseo", "강서구", "Geumjeong", "금정구",
  "Gijang", "기장군", "Haeundae", "해운대구", "Jung", "중구",
  "Nam", "남구", "Saha", "사하구", "Sasang", "사상구",
  "Seo", "서구", "Suyeong", "수영구", "Yeongdo", "영도구",
  "Yeonje", "연제구"
)

# 6. 데이터 결합
busan_final <- busan_map %>%
  left_join(name_map_busan, by = "NAME_2") %>%
  left_join(busan_cctv, by = "location")

# 7. 최종 부산 시각화 (추천: 에메랄드 청록 테마)
ggplot(busan_final) +
  geom_sf(aes(fill = total_count), color = "white", size = 0.3) +
  geom_sf_text(
    aes(
      label = location,
      # 진한 청록색(5,000대 이상) 위에만 흰색 글자 적용
      color = ifelse(total_count >= 5000, "white", "black")
    ),
    size = 3.2,
    fontface = "bold"
  ) +
  scale_color_identity() + 
  # 추천 팔레트: 맑은 민트색에서 짙은 청록(Deep Teal)까지
  scale_fill_gradientn(
    colors = c("#E0F2F1", "#80CBC4", "#26A69A", "#00897B", "#004D40"), 
    name = "CCTV 대수",
    labels = scales::comma
  ) +
  labs(
    title = "부산광역시 구별 CCTV 설치 현황",
    caption = "데이터 출처: 공공데이터포털"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = 16, color = "#004D40")
  )
