FROM racket/racket

WORKDIR /app
COPY . .
RUN apt update && apt install -y nodejs npm
RUN raco pkg install --auto parser-tools

RUN npm i
CMD ["npm", "run","dev"]
EXPOSE 3000


