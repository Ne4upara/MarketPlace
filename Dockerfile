FROM openjdk:17-alpine

WORKDIR /app

COPY gradlew .
COPY gradle gradle

COPY build.gradle settings.gradle ./

RUN ./gradlew --no-daemon dependencies

COPY src src

RUN ./gradlew --no-daemon build

EXPOSE 80

CMD ["java", "-jar", "build/libs/marketplace-0.0.1.jar"]