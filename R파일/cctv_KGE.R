# 행정안전부 CCTV 설치현황 분석

# 작업 폴더 주소 설정 및 확인
setwd('c:/R/cctv')
getwd()

# csv 파일 불러오기
file <- read.csv('CCTV.csv', header = T, fileEncoding = 'CP949')
file

# 불러온 자료 확인하기
str(file)
unique(file$설치연월)

file$연도 <- as.numeric(substr(as.character(file$설치연월), 1, 4))


# 1990년대 (1990 ~ 1999)
cctv_90s <- subset(file, 연도 >= 1990 & 연도 <= 1999)
s_90s <- sum(cctv_90s$카메라대수, na.rm = TRUE)

# 2000년대 (2000 ~ 2009)
cctv_00s <- subset(file, 연도 >= 2000 & 연도 <= 2009)
s_00s <- sum(cctv_00s$카메라대수, na.rm = TRUE)

# 2010년대 (2010 ~ 2019)
cctv_10s <- subset(file, 연도 >= 2010 & 연도 <= 2019)
s_10s <- sum(cctv_10s$카메라대수, na.rm = TRUE)

# 2020년대 (2020 ~ 2026)
cctv_20s <- subset(file, 연도 >= 2020 & 연도 <= 2026)
s_20s <- sum(cctv_20s$카메라대수, na.rm = TRUE)

line <- c(s_90s, s_00s, s_10s, s_20s)
names(line) <- c('1990~','2000~','2010~','2020~')



plot(line, type = 'o', col = 'dodgerblue4', lwd = 3, pch = 19,
     main = '연도대별 CCTV 설치 현황', xlab = '연도대', ylab = '설치 대수', xaxt = 'n')
axis(1, at = 1:4, labels = names(line))
grid() # 격자 추가


# 1. 여백 설정 (글자가 커지면 여백도 조금 더 필요합니다)
par(mar = c(6, 8, 5, 4)) 

# 2. 선 그래프 그리기
plot(line, type = 'b', 
     col = 'steelblue', lwd = 4, pch = 20, 
     main = '연도대별 CCTV 설치 현황', 
     # --- 제목 및 축 이름 설정 ---
     cex.main = 1.8,          # 제목 크기 (기본값의 1.8배)
     font.main = 2,           # 제목 굵게
     xlab = '연도대', 
     ylab = '', 
     cex.lab = 1.5,           # x, y축 이름 크기
     font.lab = 2,            # x, y축 이름 굵게
     # --------------------------
     xaxt = 'n', yaxt = 'n', las = 1,
     xlim = c(0.7, 4.3), 
     ylim = c(0, max(line) * 1.3))

# 3. y축 숫자 조절
y_labels <- seq(0, max(line) + 50000, by = 50000)
axis(2, at = y_labels, labels = format(y_labels, big.mark = ","), 
     las = 1) # 축 숫자 크기 1.2배, 굵게

# 4. x축 숫자(연도대) 조절
axis(1, at = 1:4, labels = names(line))

# 5. y축 이름 별도 표시 (위치 조정)
title(ylab = "설치 대수", line = 5.5, cex.lab = 1.5, font.lab = 2)

# 6. 그래프 위 데이터 수치 표시
text(x = 1:4, y = line, labels = format(line, big.mark = ","), 
     pos = 3, col = "black", 
     cex = 1.3,               # 숫자 크기 1.3배
     font = 2)                # 숫자 굵게

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")



# 가장 낮은 연도 (최솟값)
min(file$연도, na.rm = TRUE)

# 가장 큰 연도 (최댓값)
max(file$연도, na.rm = TRUE)
