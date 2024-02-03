FROM openjdk:17-alpine

#RUN apk --no-cache add postgresql

COPY ./build/libs/marketplace-0.0.1.jar /app/marketplace-0.0.1.jar

WORKDIR /app

CMD ["java", "-jar", "marketplace-0.0.1.jar"]