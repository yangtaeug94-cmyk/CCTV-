# 1. 필수 패키지 로드
library(sf)
library(dplyr)
library(ggplot2)
library(scales) # 숫자 콤마 표시용

# 2. 경로 설정(파일 경로 본인 걸로 설정해주세요)
setwd("C:/Users/KH/Downloads/cctv") 

# 3. 지도 및 데이터 불러오기 (한글 깨짐 방지 옵션)
korea_map <- st_read("ctprvn.shp", options = "ENCODING=CP949")
cctv_data <- read.csv("CCTV정보.csv", fileEncoding = "CP949")

# 4번 섹션: 지도가 원하는 "전라북도"로 명칭 강제 통일
cctv_stats <- cctv_data %>%
  mutate(hint = paste0(관리기관명, 소재지도로명주소, 소재지지번주소)) %>%
  mutate(sido_clean = case_when(
    grepl("서울", hint) ~ "서울특별시",
    grepl("부산", hint) ~ "부산광역시",
    grepl("대구", hint) ~ "대구광역시",
    grepl("인천", hint) ~ "인천광역시",
    grepl("광주", hint) ~ "광주광역시",
    grepl("대전", hint) ~ "대전광역시",
    grepl("울산", hint) ~ "울산광역시",
    grepl("세종", hint) ~ "세종특별자치시",
    grepl("경기", hint) ~ "경기도",
    grepl("강원", hint) ~ "강원특별자치도", # 지도가 원하는 이름
    grepl("충북|충청북도", hint) ~ "충청북도",
    grepl("충남|충청남도", hint) ~ "충청남도",
    # 전북/임실/순창/전라북도/전북특별자치도 모두 "전라북도"로 합칩니다
    grepl("전북|전라|임실|순창|전주|익산|군산", hint) ~ "전라북도", 
    grepl("전남|전라남도", hint) ~ "전라남도",
    grepl("경북|경상북도", hint) ~ "경상북도",
    grepl("경남|경상남도", hint) ~ "경상남도",
    grepl("제주", hint) ~ "제주특별자치도",
    TRUE ~ "NA"
  )) %>%
  group_by(sido_clean) %>%
  summarise(total_count = sum(카메라대수, na.rm = TRUE)) %>%
  filter(sido_clean != "NA")

# 5번 섹션: 병합 (이제 '전라북도' 키워드로 찰떡같이 붙습니다)
final_map <- korea_map %>%
  left_join(cctv_stats, by = c("CTP_KOR_NM" = "sido_clean"))

# 0. 이름 겹침 방지 패키지 설치 및 로드
#install.packages("ggrepel")
library(ggrepel)

# 6. 전국 지도 최종 (글자색 검은색 고정)
ggplot(final_map) +
  geom_sf(aes(fill = total_count), color = "white", size = 0.5) +
  geom_text_repel(
    aes(
      label = CTP_KOR_NM, 
      geometry = geometry
    ),
    stat = "sf_coordinates",
    color = "black",          # 모든 글자를 검은색으로 고정
    size = 3.5,
    fontface = "bold",        # 글씨체 굵게
    min.segment.length = Inf, # 선 제거
    box.padding = 0.3
  ) +
  scale_fill_gradientn(
    colors = c("#EBF5FB", "#AED6F1", "#5DADE2", "#2E86C1", "#1B4F72"), 
    name = "CCTV 대수",
    labels = scales::comma
  ) +
  labs(
    title = "전국 광역지자체별 CCTV 현황",
    caption = "데이터: 공공데이터포털"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = 18, color = "#2C3E50"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )
