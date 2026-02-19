# 1. 필수 패키지 로드
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# 2. 경로 설정 및 데이터 불러오기
# (경로는 컴퓨터 상황에 맞춰 한 번만 확인해 주세요!)
setwd("C:/Users/KH/Downloads/cctv") 

# 지도 데이터(JSON)와 CCTV 데이터(CSV) 로드
map <- st_read('gadm41_KOR_2.json')
cctv_data <- read.csv("CCTV정보.csv", fileEncoding = "CP949")

# 3. 지도에서 서울특별시만 추출
seoul_map <- map %>% filter(NAME_1 == "Seoul")

# 4. CCTV 데이터 전처리 및 구별 집
seoul_cctv <- cctv_data %>%
  # 관리기관명과 주소를 합쳐서 '구' 이름을 찾을 힌트 생성
  mutate(hint = paste0(관리기관명, 소재지도로명주소)) %>%
  # 서울 데이터만 필터링
  filter(grepl("서울", hint)) %>%
  # '~~구' 패턴 추출
  mutate(location = str_extract(hint, "[가-힣]+구")) %>%
  # 구별로 합계 계산
  group_by(location) %>%
  summarise(total_count = sum(카메라대수, na.rm = TRUE)) %>%
  filter(!is.na(location))

# 5. 영문(NAME_2) - 한글(location) 매칭 데이터 생성
name_map <- tibble::tribble(
  ~NAME_2, ~location,
  "Dobong","도봉구", "Dongdaemun","동대문구", "Dongjak", "동작구",
  "Eunpyeong","은평구", "Gangbuk","강북구", "Gangdong","강동구",
  "Gangnam","강남구", "Gangseo","강서구", "Geumcheon","금천구",
  "Guro","구로구", "Gwanak","관악구", "Gwangjin","광진구",
  "Jongno", "종로구", "Jung", "중구", "Jungnang", "중랑구",
  "Mapo", "마포구", "Nowon", "노원구", "Seocho", "서초구",
  "Seodaemun", "서대문구", "Seongbuk", "성북구", "Seongdong", "성동구",
  "Songpa", "송파구", "Yangcheon", "양천구", "Yeongdeungpo", "영등포구",
  "Yongsan", "용산구"
)

# 6. 지도 데이터와 CCTV 데이터 결합
seoul_final <- seoul_map %>%
  left_join(name_map, by = "NAME_2") %>%
  left_join(seoul_cctv, by = "location")

# 7. 최종 시각화 출력 (초록색 테마 + 구 이름 중앙 고정)
ggplot(seoul_final) +
  geom_sf(aes(fill = total_count), color = "white", size = 0.3) +
  # 구 이름을 구 정중앙에 배치
  geom_sf_text(
    aes(
      label = location,
      # CCTV 12,000대 이상인 진한 곳은 흰색 글씨, 나머지는 검은색
      color = ifelse(total_count >= 12000, "white", "black")
    ),
    size = 3.2,
    fontface = "bold"
  ) +
  scale_color_identity() + 
  # 팀장님 취향 저격 초록색 스펙트럼
  scale_fill_gradientn(
    colors = c("#F7FCF5", "#C7E9C0", "#74C476", "#238B45", "#00441B"), 
    name = "CCTV 대수",
    labels = scales::comma
  ) +
  labs(
    title = "서울특별시 구별 CCTV 설치 현황",
    caption = "데이터 출처: 공공데이터포털"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = 16, color = "#00441B")
  )