
# [ 카이제곱분포 ]
# - 분산 추정과 관련된 분포 : 모분산 추정 및 가설검정에 사용
# - 적합도 검정 : 어떤 분포를 따르는지 검정
#                 (정규성 검정, 카이제곱검정 - 기대분포를 따르는지)
# - 비대칭형
# - 모수 : 자유도
# - 자유도가 클수록 중심이 오른쪽으로 이동(대칭성 강화)


# 카이제곱 분포 시각화
vx <- seq(0, 50, 0.01)
vy1 <- dchisq(vx, df = 1)
vy2 <- dchisq(vx, df = 5)
vy3 <- dchisq(vx, df = 10)

dev.new()
plot(vx, vy1, type = 'l', col = 1, ylim = c(0, 0.5))
lines(vx, vy2, type = 'l', col = 2)
lines(vx, vy3, type = 'l', col = 3)


# 카이제곱 통계량과 분포
# 1. 표준정규분포를 따르는 표본 k를  추출,
#    X**2 ~ chisq(1)

# [ 증명 ]
# 1) 난수 생성
set.seed(0)
vx1 <- rnorm(100, 0, 1)

# 2) 실제 분포 시각화
#    표준정규분포를 따르는 100개의 난수 제곱값의 실제 분포 히스토그램
dev.new()
hist(vx1**2, prob = T)           

# 3) 이론 분포 시각화
vx2 <- seq(0, 6, 0.01)
vy2 <- dchisq(vx2, df = 1)
lines(vx2, vy2, type = 'l', col = 2) 



# 2. 표준정규분포를 따르는 표본 k를 추출,
#    Σ(X**2) ~ chisq(k)

# [ 증명 ]
# 1) 난수 생성
set.seed(0)
vx1 <- rnorm(100, 0, 1)
sum(vx1**2)

# 2) 여러개의 난수 생성
vchi <- c()
for (i in 1:1000) {
  vx1 <- rnorm(100, 0, 1)
  vchi <- c(vchi, sum(vx1**2))
}

# 3) 실제 분포
dev.new()
hist(vchi, prob = T, ylim = c(0, 0.03)) 

# 4) 이론 분포 시각화
vx3 <- seq(60, 160, 0.01)
vy3 <- dchisq(vx3, df = 100)
lines(vx3, vy3, type = 'l', col = 2) 



# 모분산 추정 시 사용하는 통계량
V = (n-1)*S**2 / σ**2

P(chisq(0.025) < V < chisq(0.975)) = 95%
P(chisq(0.025) < (n-1)*S**2 / σ**2 < chisq(0.975)) = 95%

1 / Chisq(0.975) <  σ^2 / (n-1)*S^2 < 1 / Chisq(0.025)
(n-1)*S^2 / Chisq(0.975)   <  σ^2   < (n-1)*S^2 / Chisq(0.025)


# 2) 모분산 점추정 : E(S**2) = σ^2
# 2) 모분산 구간추정 : [(n-1)*S**2 / chisq(0.975) , 
#                       (n-1)*S**2 / chisq(0.025)]



# 예제) 모분산 가설 검정
# 평균 0, 분산이 1인 표준정규분포를 따르는 임의 샘플 50개를 사용하여
# 분산이 2인지 가설 검정

H0 : σ**2 = 2
H1 : σ**2 != 2
  
# step 1 ) 난수 추출
set.seed(0)
vx <- rnorm(50, 0, 1)
vs2 <- var(vx)                      # 표본분산(0.839596)
n <- 50


# step 2) 신뢰구간 및 검정통계량
ld <- qchisq(0.025, df = 49)        # 31.55492
lm <- qchisq(0.975, df = 49)        # 70.22241

# 1) 신뢰구간(채택역)
c((n-1)* vs2 / lu , (n-1)* vs2/ ld) # [0.5858558, 1.3037653]

# 2) 검정통계량
V = (n-1) * S**2 / σ**2
  = (n-1) * vs2 / 2
  = 20.5701
# 검정통계량 채택역 : [31.55492, 70.22241] 
# 귀무가설이 검정통계량 채택역에 포함되어 있지 않으므로 귀무가설 기각

# 3) 유의확률
p-value = P(V < v*)
        = P(V < 20.5701)
        = pchisq(20.5701, df=49)
        = 0.0001149341
# 유의확률이 기준인 0.025보다 작으므로 귀무가설 기각


# step 3) 모분산검정(VarTest)
library(DescTools)
VarTest(x,                             
        y,                             # 두집단 분산 비교시 사용
        alternative = c("two.sided",   # 양측검정
                        "less",        # 왼쪽검정
                        "greater"),    # 오른쪽검정
        ratio = 1,                     # 분산비(두집단 분산 비교시 사용)
        sigma.squared = 1,             # 한 집단 분산 추정 시 가정  
        conf.level = 0.95, ...)

VarTest(vx,  sigma.squared = 2)

# One Sample Chi-Square test on variance
# 
# data:  vx
# X-squared = 20.57, df = 49, p-value = 0.0002299
# alternative hypothesis: true variance is not equal to 2
# 95 percent confidence interval:
#   0.5858558 1.3037653
# sample estimates:
#   variance of x 
# 0.839596 



# [ 연습 문제 ]
# 정규모집단으로부터 표본의 크기가 25인 표본을 추출하였다.
# 이 표본의 표본분산이 20일 때, 모분산의 95% 신뢰구간을 구하세요



# [ 연습 문제 ]
# 어떤 제품의 정확한 무게를 알아보기 위해 7개의 제품을 임의로 추출하여 무게를 측정한
# 결과가 다음과 같았다. 이 때 이 측정치의 분포가 정규분포를 따른다고 가정하고,
# 모분산에 대한 95% 신뢰구간을 구하라.

# [ 45, 47, 44, 48, 46, 45, 47 ]




