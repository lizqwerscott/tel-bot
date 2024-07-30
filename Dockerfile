# 使用 Alpine 作为基础镜像
FROM alpine:latest

# 更新包列表并安装 Python 和 pip
RUN apk add --no-cache \
    python3 \
    py3-pip \
    sbcl \
    bash \
    git \
    curl

# 设置时区为上海
RUN apk add tzdata && cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime \
    && echo "Asia/Shanghai" > /etc/timezone \
    && apk del tzdata

# 安装 quicklisp
WORKDIR /root
RUN wget https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --non-interactive --load ./quicklisp.lisp --eval "(quicklisp-quickstart:install)"
COPY ./docker/.sbclrc /root

# 安装项目依赖
WORKDIR /root/quicklisp/local-projects/
RUN git clone https://github.com/lizqwerscott/cl-telegram-bot.git && \
    git clone https://github.com/lizqwerscott/lzputils.git && \
    git clone https://github.com/lizqwerscott/easy-config.git

# 复制应用代码
COPY . /root/quicklisp/local-projects/tel-bot

# 设置工作目录
RUN mkdir -p /root/downloads
WORKDIR /root/quicklisp/local-projects/tel-bot

# 安装 lisp 依赖
RUN sbcl --non-interactive --eval "(ql:quickload :tel-bot)"
