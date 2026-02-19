# 1. 패키지 로드
library(dplyr)
library(ggplot2)
library(treemapify)
library(patchwork) # 그래프 합치기용

# 2. 데이터 불러오기 및 전처리
data <- read.csv('C:\\R\\CCTV정보.csv', fileEncoding = 'CP949')

# 컬럼명 통일 (기존 데이터와 맞춤)
names(data)[names(data) == "설치목적구분"] <- "설치목적" 
names(data)[names(data) == "카메라대수"] <- "카메라대수"

# '지역' 컬럼 생성 (관리기관명에서 앞 2글자)
data$지역 <- substr(data$관리기관명, 1, 2)
# '카메라대수' NA 값 0으로 처리 및 numeric 변환
data$카메라대수 <- as.numeric(data$카메라대수)
data$카메라대수[is.na(data$카메라대수)] <- 0

# ---------------------------------------------------------
# [데이터 요약] - 모든 그래프의 기반 데이터
# ---------------------------------------------------------
# 1) 전국 설치목적별 건수 및 비율 (4번 분석용)
purpose_count_all <- data %>%
  group_by(설치목적) %>%
  summarise(count = n()) %>%
  mutate(ratio = (count / sum(count)) * 100) %>%
  arrange(desc(ratio))

# 2) 전국 설치목적별 카메라 총 대수 및 비율 (5번 분석용)
purpose_camera_all <- data %>%
  group_by(설치목적) %>%
  summarise(total_units = sum(카메라대수, na.rm = TRUE)) %>%
  mutate(ratio = (total_units / sum(total_units)) * 100) %>%
  arrange(desc(ratio))

# 3) 지역별 설치목적별 카메라 총 대수 (지역별 분석용)
region_purpose_camera <- data %>%
  group_by(지역, 설치목적) %>%
  summarise(총대수 = sum(카메라대수, na.rm = TRUE), .groups = 'drop') %>%
  arrange(지역, desc(총대수))

# ---------------------------------------------------------
# [그래프 세트 1] 4. 전국 설치목적별 빈도(건수) 비율
# ---------------------------------------------------------
g1_1 <- ggplot(purpose_count_all, aes(area = count, fill = 설치목적, 
                                      label = paste(설치목적, paste0(round(ratio, 1), "%"), sep="\n"))) +
  geom_treemap(color = "white", size = 1.5) +
  # colour = "black"으로 수정하여 가독성 확보
  geom_treemap_text(colour = "black", place = "centre", size = 12, 
                    family = "Malgun Gothic", fontface = "bold") +
  # 파스텔톤 팔레트
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "전국 CCTV 설치목적별 건수 비율", 
       caption = "전체 설치 건수 기준") +
  theme(text = element_text(family = "Malgun Gothic"), 
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "right") # 오른쪽에 범례 유지

print(g1_1)


# 1. 숫자 지수 표기법 방지 설정
options(scipen = 999) 
# 2. 숫자 콤마 표시를 위한 패키지 (설치 안 되어 있으면: install.packages("scales"))
library(scales)

# ---------------------------------------------------------
# [그래프 세트 2] 5. 전국 CCTV 설치목적별 카메라 갯수
# ---------------------------------------------------------
g2_bar_final <- ggplot(purpose_camera_all, aes(x = reorder(설치목적, total_units), y = total_units, fill = 설치목적)) +
  # 막대 그래프 생성
  geom_bar(stat = "identity", color = "white", width = 0.7) +
  # 가로 막대 전환 (항목 이름 가독성 확보)
  coord_flip() +
  
  # 막대 끝에 실제 갯수 표시 (콤마 포함)
  geom_text(aes(label = paste0(format(total_units, big.mark=","), "개")), 
            hjust = -0.1, size = 4.5, family = "Malgun Gothic", fontface = "bold") +
  
  # 디자인 설정
  scale_fill_brewer(palette = "Set3") + 
  # 하단 축 숫자를 1e+05가 아닌 100,000 형태로 표시
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .15))) + 
  
  labs(title = "전국 목적별 CCTV 갯수", 
       subtitle = "전체 CCTV 갯수 기준",
       x = "설치 목적", 
       y = "CCTV 갯수 (단위: 개)",
       caption = "데이터 출처: 공공데이터포털 기반 전처리 자료") +
  
  theme_minimal() +
  theme(
    text = element_text(family = "Malgun Gothic"),
    # 제목 스타일 (CCTV 글자 뭉침 방지)
    plot.title = element_text(size = 20, face = "plain", hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    # 축 글자 크기 조절
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    # 불필요한 범례 제거
    legend.position = "none",
    # 배경 가이드라인을 옅게 넣어 빈 공간 느낌 제거
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90")
  )

print(g2_bar_final)




































