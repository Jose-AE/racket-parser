FROM racket/racket

WORKDIR /app
COPY . .
RUN apt update && apt install -y nodejs npm
RUN npm i
CMD ["npm", "run","dev"]
EXPOSE 3000

