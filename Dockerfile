FROM racket/racket

WORKDIR /app

COPY package*.json ./

RUN apt update && apt install -y nodejs npm
RUN npm i
COPY . .

RUN raco pkg install --auto parser-tools


CMD ["npm", "run","dev"]
EXPOSE 3000


