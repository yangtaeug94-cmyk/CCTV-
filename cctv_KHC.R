# csv 파일을 이용해 데이터를 읽고 쓰기
# 데이터 읽기
getwd()
# setwd('C:\\R\\ch06')
setwd('C:/project3-cctv')
getwd()

# 필수 패키지 로드
if (!require(plotrix)) install.packages("plotrix")
if (!require(dplyr)) install.packages("dplyr")
library(plotrix)
library(dplyr)

# 데이터 불러오기
# 실제 파일 경로를 입력하세요. (한글 깨짐 방지를 위해 fileEncoding 확인 필수)
df <- read.csv("CCTV정보.csv", fileEncoding = "CP949")

# 데이터 정제 (시도 단위로 묶기) ---
# 관리기관명이 제각각이라 '키워드'를 기준으로 큰 단위로 합칩니다.
df_sido <- df %>%
  mutate(시도 = case_when(
    grepl("서울", 관리기관명) | grepl("금천구청", 관리기관명) ~ "서울특별시",
    grepl("부산", 관리기관명) ~ "부산광역시",
    grepl("대구", 관리기관명) ~ "대구광역시",
    grepl("인천", 관리기관명) ~ "인천광역시",
    grepl("광주", 관리기관명) ~ "광주광역시",
    grepl("대전", 관리기관명) ~ "대전광역시",
    grepl("울산", 관리기관명) ~ "울산광역시",
    grepl("세종", 관리기관명) ~ "세종특별자치시",
    grepl("경기", 관리기관명) ~ "경기도",
    grepl("강원", 관리기관명) ~ "강원도",
    grepl("충북|충청북도", 관리기관명) ~ "충청북도",
    grepl("충남|충청남도", 관리기관명) ~ "충청남도",
    grepl("전북|전라북도", 관리기관명) ~ "전라북도",
    grepl("전남|전라남도", 관리기관명) ~ "전라남도",
    grepl("경북|경상북도", 관리기관명) ~ "경상북도",
    grepl("경남|경상남도", 관리기관명) ~ "경상남도",
    grepl("제주", 관리기관명) ~ "제주특별자치도",
    TRUE ~ "기타"
  ))

# 시도별 통계 계산 ---
sido_stats <- df_sido %>%
  group_by(시도) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# 확인
print(sido_stats)

df_sido %>% 
  filter(시도 == "기타") %>% 
  group_by(관리기관명) %>% 
  summarise(n = n())

# 2. 비중 계산 및 상위 10개 외 '기타' 합치기 
top_n_limit <- 10  # 상위 10개만 개별 표시

if (nrow(sido_stats) > top_n_limit) {
  top_data <- sido_stats %>% head(top_n_limit)
  other_data <- sido_stats %>% slice((top_n_limit + 1):n()) %>%
    summarise(시도 = "나머지", count = sum(count))
  plot_data_final <- rbind(top_data, other_data)
} else {
  plot_data_final <- sido_stats
}

# 3. 도넛 차트 좌표 및 라벨 계산
plot_data_final <- plot_data_final %>%
  mutate(
    fraction = count / sum(count),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    label_pos = (ymax + ymin) / 2,
    label = paste0(시도, "\n", round(fraction * 100, 1), "%")
  )

# 4. 세련된 도넛 차트 그리기
ggplot(plot_data_final, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=시도)) +
  geom_rect(color="white", size=0.3) + 
  coord_polar(theta="y") + 
  xlim(c(1, 6)) + 
  # mako, turbo, rocket 등 다양한 옵션을 써보세요!
  scale_fill_viridis_d(option = "turbo") + 
  theme_void() + 
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b=20)),
    plot.margin = unit(c(1,1,1,1), "cm")
  ) +
  geom_text_repel(aes(x=4, y=label_pos, label=label), 
                  size=3.8, fontface="bold", 
                  nudge_x = 1.3, segment.color = "grey50") +
  annotate("text", x = 1, y = 0, 
           label = paste0("전국 총합\n", format(sum(plot_data_final$count), big.mark=","), "개"), 
           size = 5, fontface = "bold", color = "#2c3e50") +
  labs(title = "전국 주요 시·도별 CCTV 관리 비중")

# 시도별 비중 하위 10개 출력
sido_stats %>% 
  arrange(desc(count)) %>%  # 큰 순서대로 정렬 후
  tail(10)                  # 맨 아래 10개만 추출


# 1. 필수 패키지 로드
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
library(ggplot2)
library(dplyr)

# 패키지 설치 (인터넷 연결 필요)
install.packages("ggrepel")

# 패키지 불러오기
library(ggrepel)

# 2. 서울 데이터만 추출 및 정제
# '서울특별시' 키워드가 들어간 기관만 필터링합니다.
seoul_data <- df %>%
  filter(grepl("서울특별시", 관리기관명) | grepl("서울시", 관리기관명) | grepl("금천구청", 관리기관명)) %>%
  mutate(구명 = gsub("서울특별시 ", "", 관리기관명)) %>% # '서울특별시' 글자 제거해서 이름 단축
  mutate(구명 = gsub("청", "", 구명)) %>%            # '구청' 글자 제거 (예: 종로구청 -> 종로구)
  group_by(구명) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# 비율 계산 및 라벨 만들기
seoul_data <- seoul_data %>%
  arrange(desc(count)) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         label_pos = (ymax + ymin) / 2,
         label = paste0(구명, "\n", round(fraction * 100, 1), "%"))

# 도넛 차트 그리기
ggplot(seoul_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=구명)) +
  geom_rect(color="white", size=0.2) +      # 구분선을 더 얇게 수정
  coord_polar(theta="y") + 
  xlim(c(1, 6)) +                           # xmin을 1로 조정하여 도넛 구멍을 세련된 크기로 변경
  theme_void() + 
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b=20)),
    plot.background = element_rect(fill = "white", color = NA) # 배경을 깔끔하게 화이트로
  ) +
  # 1. 색상 팔레트 변경 (Set3 또는 Spectral 사용 시 훨씬 전문적임)
  scale_fill_manual(values = colorRampPalette(c("#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F", "#8CD17D"))(nrow(seoul_data))) +
  
  # 2. 선과 라벨 디자인 고도화
  geom_text_repel(aes(x=4, y=label_pos, label=label), 
                  size=3.2, 
                  fontface="bold",          # 글자를 진하게
                  color="#333333",          # 진한 회색으로 가독성 향상
                  nudge_x = 1.5,            # 선을 조금 더 밖으로
                  segment.size = 0.4,
                  segment.color = "grey70") +
  
  # 3. 도넛 중앙에 타이틀과 로고 역할의 텍스트 추가
  annotate("text", x = 1, y = 0, 
           label = paste0("서울시 전체\n", sum(seoul_data$count), "개 지점"), 
           size = 5, fontface = "bold", color = "#2c3e50") +
  
  labs(title = "서울시 자치구별 CCTV 관리 지점 현황")

# 부산 데이터 추출 및 구 이름 정제
# 1. 데이터 정제
busan_data <- df %>%
  filter(grepl("부산", 관리기관명)) %>%
  mutate(구명 = case_when(
    grepl("부산진구", 관리기관명) ~ "부산진구",
    grepl("기장군", 관리기관명) ~ "기장군",
    grepl("강서구", 관리기관명) ~ "강서구",
    TRUE ~ gsub("부산광역시 ", "", 관리기관명)
  )) %>%
  mutate(구명 = gsub("청|\\(.*?\\)", "", 구명)) %>%
  mutate(구명 = trimws(구명)) %>%
  group_by(구명) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(
    fraction = count / sum(count),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    label_pos = (ymax + ymin) / 2,
    label = paste0(구명, " ", round(fraction * 100, 1), "%")
  )

# 2. 선을 길게 뽑아 시원하게 펼친 시각화
ggplot(busan_data, aes(ymax=ymax, ymin=ymin, xmax=5, xmin=4.5, fill=구명)) +
  geom_rect(color="white", size=0.4) +
  coord_polar(theta="y") + 
  
  # xlim의 범위를 10 이상으로 늘려 라벨이 멀리 나갈 '운동장'을 크게 만듭니다.
  xlim(c(-2, 11)) + 
  
  scale_fill_viridis_d(option = "mako", direction = -1) + 
  theme_void() + 
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold", margin = margin(t=20, b=30)),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  
  # 라벨 설정: 선이 절대 뭉개지지 않도록 nudge와 force 최적화
  geom_text_repel(aes(x=5.2, y=label_pos, label=label), 
                  size=3.8, 
                  fontface="bold", 
                  color="#333333",
                  nudge_x = 4.5,             # 선을 훨씬 길게 밖으로 뽑음 (가장 중요!)
                  segment.size = 0.5,       # 선 두께를 살짝 키워 선명하게
                  segment.color = "grey50", 
                  min.segment.length = 0,   # 모든 항목에 선 강제 생성
                  direction = "y",          # 좁은 구간은 위아래로 넓게 분산
                  box.padding = 0.5,        # 라벨끼리 밀어내는 간격
                  point.padding = 0.2,
                  force = 1.5) +            # 라벨이 겹치지 않게 밀어내는 힘 강화
  
  # 중앙 텍스트 (공간을 매우 넓게 써서 시원한 느낌 강조)
  annotate("text", x = -2, y = 0, 
           label = paste0("BUSAN\n", format(sum(busan_data$count), big.mark=","), "개"), 
           size = 8, fontface = "bold", color = "#2c3e50", lineheight = 0.8) +
  
  labs(title = "부산광역시 자치구별 CCTV 관리 현황")

print(busan_data)
