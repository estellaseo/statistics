# [ 두 집단 모평균 비교 가설 검정 ]
# 두 집단의 평균이 같은지 혹은 어느쪽이 더 큰지에 대한 가설검정

# 1. 독립 표본
#    - 두 집단이 서로 독립적
#    - ex) 성별 시험성적 차이

# 1) 양측 검정
#    H0 : mu1 = mu2(mu1 - mu2 = 0)
#    H1 : mu1 != mu2(mu1 - mu2 != 0)

# 2) 왼쪽 검정
#    H0 : mu1 = mu2(mu1 - mu2 = 0)
#    H1 : mu1 < mu2(mu1 - mu2 < 0)

# 3) 오른쪽 검정
#    H0 : mu1 = mu2(mu1 - mu2 = 0)
#    H1 : mu1 > mu2(mu1 - mu2 > 0)


# 2. 대응 표본
#    - 처리 전, 후 비교(처리 효과)
#    - 동일한 관찰 대상으로부터 처리를 실시하기 전, 후 비교
#    - ex) 다이어트 약 효과




# [ 독립 표본에 대한 가설 검정]
# xbar1 sample size : n
# xbar2 sample size : m

xbar1 - xbar2 ~ N(μ1 - μ2, σ1**2/n + σ2**2/m)
E(xbar1 - xbar2) = E(xbar1) - E(xbar2) 
= mu1 - mu2

var(xbar1 - xbar2) = var(xbar1) + var(xbar2)
= σ1**2/n + σ2**2/m


xbar1 - xbar2 ~ N(μ1 - μ2, σ1**2/n + σ2**2/m)

Z* = ((xbar1 - xbar2) - (μ1 - μ2)) / (σ * sqrt(1/n + 1/m)) ~ N(0,1)
t* = ((xbar1 - xbar2) - (μ1 - μ2)) / (s * sqrt(1/n + 1/m)) ~ T(n+m-2)

s = sqrt( ((n-1)*s1^2 + (m-1)*s2^2) / (n + m - 2))


# 예제) weight.txt에서 남녀 몸무게 차이가 있는지 가설검정(유의수준 5%)
v1 <- read.table('data/weight.txt', sep = ' ', header = T)

# 가설
H0 : mu남 - mu여 = 0
H1 : mu남 - mu여 != 0

df1_m <- v1[v1$gender == 1, 'weight']
df1_f <- v1[v1$gender == 2, 'weight']

# 필요 변수 정의
xbar1 <- mean(df1_m)
xbar2 <- mean(df1_f)

n <- length(df1_m)
m <- length(df1_f)

σ <- 500

# 1) Z 검정
# 1-1) 검정 통계량
Z* = (xbar1 - xbar2) / (σ * sqrt(1/n + 1/m))
   = -1.584121

# 채택역 : [-1.96, 1.96]
# > 검정통계량이 채택역 구간 안에 포함되어 있으므로 귀무가설 채택 


# 1-2) p-value
P(Z < -1.584121)
pnorm(-1.584121, 0, 1)             # 0.05658309 > 0.025
# > 유의확률이 양측검정 시 기준인 0.025보다 크기 때문에 귀무가설 채택


# 1-3) z-test
library(BSDA)
z.test(df1_m, df1_f, mu = 0, sigma.x = 500, sigma.y = 500)




# 2) T 검정
# 2-1) 검정통계량
s1 <- sd(df1_m)
s2 <- sd(df1_f)

s <- sqrt( ((n-1)*s1^2 + (m-1)*s2^2) / (n + m - 2))

t* = ((xbar1 - xbar2) - (μ1 - μ2)) / (s * sqrt(1/n + 1/m)) ~ T(n+m-2)
   = ((xbar1 - xbar2)) / (s * sqrt(1/n + 1/m)) 
   = -1.522856

qt(0.05/2, df = n+m-2)
# 채택역 : [-2.018082, 2.018082]
# > 귀무가설 채택


# 2-2) p-value
P(T < -1.522856)
pt(-1.522856, df =  n+m-2) = 0.06764465 > 0.05/2


# 2-3) t.test
t.test(df1_m, df1_f, mu = 0, var.equal = T)

































