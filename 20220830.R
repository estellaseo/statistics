# [ 정규분포 ]
# 연속형 확률 변수에 대한 분포
# 확률 밀도 함수
# 평균 기준 좌우 대칭의 모양을 띄고 있음 (종모양)
# 모수(분포를 결정 짓는 요인) : 평균, 분산(σ^2)
# X ~ N(µ, σ^2)

# 시각화
# 1) 평균 : 10, 분산 : 25인 정규분포 시각화
vx <- seq(-10, 30, 0.01)
vy <- dnorm(vx, mean = 10, sd = 5)

dev.new()
plot(vx, vy, type = 'l', ylim = c(0, 0.2))

# 2) 평균 : 10, 분산 : 10인 정규분포 시각화
vy2 <- dnorm(vx, mean = 10, sd = sqrt(10))
lines(vx, vy2, type = 'l', col = 'blue')

# 3) 평균 : 10, 분산 : 50인 정규분포 시각화
vy3 <- dnorm(vx, mean = 10, sd = sqrt(50))
lines(vx, vy3, type = 'l', col = 'red')

# 평균에 의해 좌우 이동
# 분산에 의해 뾰족함의 정도(첨도)가 달라짐
# 분산이 커질수록 완만, 작을수록 뾰족해지는 형태




# [ 표준 정규 분포 ]
# 정규분포를 따르는 확률변수를 표준화한 분포
# 평균 : 0, 표준편차 : 1인 정규분포
# Z = (X - µ) / σ
# Z ~ N(0, 1)

vx <- seq(-3, 3, 0.01)
vy <-  dnorm(vx, mean = 0, sd = 1)
plot(vx, vy, type = 'l')


# 예제) X ~ N(170, 100)인 100개의 난수를 추출할 때, 
#      해당 난수가 갖는 실제 분포는 무엇인가?
v1 <- rnorm(100, mean = 170, sd = sqrt(100))
z1 <- (v1 - 170) / 10

# 표준화된 난수의 실제 분포(히스토그램)
hist(z1, probability = T, ylim = c(0, 0.5))
lines(vx, vy, type = 'l', col = 'red')



# 표준정규분포 95%, 99% 범위 시각화
ld <- qnorm(0.01/2, mean = 0, sd = 1)
lu <- qnorm(1-0.01/2, mean = 0, sd = 1)

plot(vx, vy, type = 'l')

# 임계값 수직선 그리기
abline(v = ld)
abline(v = lu)

# 색 채우기
vx1 <- c(ld, seq(ld, lu, 0.01), lu)
vy1 <- c(0, dnorm(seq(ld, lu, 0.01), mean = 0, sd = 1), 0)

polygon(x,                 
        y, 
        density = , 
        angle = , 
        col = )

polygon(vx1, vy1, density = 30, col = 'pink', border = 'black')

# 텍스트 삽입
text(0, 0.2, '99%', cex = 1.5)



# [ 연습 문제 ]
# 표준정규분포 95% 임계값을 찾아 좌우 영역 구분하여 시각화
dev.new()
vx <- seq(-3, 3, 0.01)
vy <-  dnorm(vx, mean = 0, sd = 1)
plot(vx, vy, type = 'l')

ld2 <- qnorm(0.05/2, mean = 0, sd = 1)
lu2 <- qnorm(1-0.05/2, mean = 0, sd = 1)

vx2 <- c(-3, seq(-3, ld2, 0.01), ld2)
vy2 <- c(0, dnorm(seq(-3, ld2, 0.01), mean = 0, sd = 1), 0)
vx3 <- c(lu2, seq(lu2, 3, 0.01), 3)
vy3 <- c(0, dnorm(seq(lu2, 3, 0.01), mean = 0, sd = 1), 0)

polygon(vx2, vy2, density = 30, col = 'red', border = 'black')
polygon(vx3, vy3, density = 30, col = 'red', border = 'black')
text(0, 0.2, '95%', cex = 1.5)

# 화살표
arrows(x0,                     # 시작점 x축 좌표
       y0,                     # 시작점 y축 좌표
       x1,                     # 끝점 x축 좌표
       y1,                     # 끝점 y축 좌표
       length = ,              # 촉 길이
       angle = ,
       col= ,
       lty = ,
       lwd = )

arrows(-2.5, 0.05, -2.3, 0.01, length = 0.1, col = 2)
text(-2.5, 0.06, '0.025', cex = 0.5)
