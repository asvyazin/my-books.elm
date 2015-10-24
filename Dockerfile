FROM asvyazin/elm

MAINTAINER Alexander Svyazin <guybrush@live.ru>

WORKDIR /src
COPY . /src

RUN npm install -y
RUN npm run elm-package -- install -y
RUN npm run elm-make -- Main.elm Login.elm
RUN npm run jspm -- install -y
