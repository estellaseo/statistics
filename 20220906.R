# [ 단측 검정 ]
# 1. 왼쪽 검정 : 기각역이 왼쪽에 있는 형태
#    1) 신뢰구간
#       P(Z >= -1.64) = 95%
#       P((xbar - mu) / (sigma/sqrt(n)) >= -1.64) = 95%
#       채택역 [-inf, xbar + 1.64 * sigma/sqrt(n)]
#       기각역 [xbar + 1.64 * sigma/sqrt(n), inf]

#    2) 검정통계량에서의 기각역과 채택역
#       기각역 : [-inf, -1.64]
#       채택역 : [-1.64, inf]

#    3) 유의확률
#       p-value(Z < z*)




# [ 연습 문제 ]
# 여아 신생아 파일을 읽고 다음 가설에 대한 가설 검정 수행(유의수준 : 5%)
# 작년 여아 신생아 몸무게는 2800으로 알려져있다. (sigma = 500 가정)
# 올해는 작년에 비해 증가했을 것으로 예상된다.

# H0 : 기존에 알려져 있는 사실
# H1 : 연구자가 밝히려고 하는 사실

v1 <- scan('data/여아신생아.txt')
xbar <- mean(v1)
n <- length(v1)
sigma <- 500

# 1. 오른쪽 검정
# H0 : mu = 2800
# H1 : mu > 2800 

# 1) 신뢰구간
mu >= xbar - 1.64 * sigma/sqrt(n)
#    채택역 : [xbar - 1.64 * sigma/sqrt(n), inf]    [2939.169, inf]
#    기각역 : [-inf, xbar - 1.64 * sigma/sqrt(n)]   [-inf, 2939.169]
#    귀무가설이 기각역 안에 포함되어 있으므로 귀무가설 기각

# 2) 검정통계량
z* = (xbar - 2800) / (sigma/sqrt(n))
   = 2.820885

#    채택역 : [-inf, 1.645]
#    기각역 : [1.645, inf]


# 3) 유의확률
p-value = P(Z > z*)
        = P(Z > 2.820885)
        = 1 - P(Z < 2.820885)
        = 1 - pnorm(2.820885, 0, 1)
        = 0.002394568 <<< 0.05



# 2. 왼쪽 검정
# H0 : mu = 2800
# H1 : mu < 2800

# 1) 신뢰구간
mu <= xbar + 1.64 * sigma/sqrt(n)
#    채택역 : [-inf, xbar + 1.64 * sigma/sqrt(n)]    [-inf, 3325.72]
#    기각역 : [xbar + 1.64 * sigma/sqrt(n), inf]     [3325.72, inf]
# > 귀무가설이 채택역 구간에 포함되어 있으므로 귀무가설 채택

# 2) 검정통계량
z* = (xbar - 2800) / (sigma/sqrt(n))
   = 2.820885

# 3) 유의확률
p-value = P(Z < z*)
        = P(Z < 2.820885)
        = pnorm(2.820885, 0, 1)
        = 0.9976054 >>>> 0.05

# 4) z-test
library(BSDA)
z.test(v1, alternative = 'less',
       # default : two.sided(양측검정), less : 왼쪽검정, greater :오른쪽 검정
       mu = 2800, sigma.x = 500)




# [ 연습 문제 ]
# 아래에 대한 가설 검정을 수행하여라 
# A사 K모델 자동차의 연비는 평균 12.5(km/l), 표준편차 0.5(km/l)로 알려져 있는데, 
# 새로 개발된 엔진을 장착한 총 40대의 자동차 연비를 측정한 결과 평균이 12.64(km/l)로 나왔다.
# 기존 연비보다 개선됐는지 가설검정(유의수준 5%)
xbar <- 12.64
sigma <- 0.5
n <- 40

# H0 : mu = 12.5
# H1 : mu > 12.5 (오른쪽 검정)

# 1)신뢰구간
mu <= xbar - 1.64 * sigma/sqrt(n) = 12.51035
#   기각역 : [12.51035, inf]
#   기각역 : [-inf, 12.51035]
#   > 귀무가설이 기각역 구간에 포함되어 있으므로 귀무가설 기각


# 2) 검정통계량
Z = (xbar - mu) / (sigma/sqrt(n))
z* = (12.64 - 12.5) / (sigma/sqrt(n)) = 1.770875
  
#    채택역 : [-inf, 1.64] 
#    기각역 : [1.64, inf]
#    > 검정통계량 값이 기각역에 포함되어 있으므로 귀무가설 기각
  
# 3) 유의확률 
p-value = P(Z > z*)
        = P(Z > 1.770875)
        = 1 - P(Z < 1.770875)
        = 1 - pnorm(1.770875, 0, 1)
        = 0.03829075 < 0.05
#    > 유의확률이 유의수준(0.05)보다 작기 때문에 귀무가설 기각
  
# 4) z-test
library(BSDA)
v2 <- replicate(40, 12.64)
z.test(v2, alternative = 'greater', mu = 12.5, sigma.x = 0.5)

# One-sample z-Test
# 
# data:  v2
# z = 1.7709, p-value = 0.03829
# alternative hypothesis: true mean is greater than 12.5
# 95 percent confidence interval:
#   12.50996       NA
# sample estimates:
#   mean of x 
# 12.64 



# [ 연습 문제 ] 단측 검정
# 기존 치료법의 치료기간이 평균 10일, 표준편차가 3일인 정규분포를 따른다고 알려져 있다. 
# 새로운 치료법을 25명의 환자에게 적용해서 평균 9일, 표준편차가 3일인 성적을 얻었다.
# 이 경우, 새로운 치료법의 효과를 인정할 수 있는가(치료기간이 작아짐)
# 유의수준 : 5%  
xbar <- 9
sigma <- 3
n <- 25

# 1. 가설
H0 : mu = 10
H1 : mu < 10  (왼쪽 검정)

# 2. 가설 검정
# 1) 신뢰구간
mu < xbar - 1.64 * sigma/sqrt(n) = 8.016

#     채택역 : [-inf, 8.016]
#     기각역 : [8.016, inf]
#     > 귀무가설이 기각역 안에 포함되어 있으므로 귀무가설 기각

# 2) 검정통계량
Z = (xbar - mu) / (sigma/sqrt(n))
z* = (9 - 10) / (sigma/sqrt(n)) = -1.666667

#    채택역 : [1.64, inf]
#    기각역 : [-inf, 1.64]

#    > 검정통계량 값이 기각역에 포함되어 있으므로 귀무가설 기각

# 3) 유의확률
p-value = P(Z < z*)
        = P(Z < -1.666667)
        = pnorm(-1.666667, 0, 1)
        = 0.04779032
#    > 유의확률이 유의수준(0.05)보다 작기 때문에 귀무가설 기각

# 3. z.test
v1 <- replicate(25, 9)
z.test(v1, alternative = 'less', mu = 10, sigma.x = 3)

# One-sample z-Test
 
# data:  v1
# z = -1.6667, p-value = 0.04779
# alternative hypothesis: true mean is less than 10
# 95 percent confidence interval:
#   NA 9.986912
# sample estimates:
#   mean of x 
# 9 
