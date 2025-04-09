# Import base image
FROM python:3.11

# Set arguments and environment variables
ENV PIP_DISABLE_PIP_VERSION_CHECK=1
ENV PIP_NO_CACHE_DIR=1
ENV POETRY_VERSION=1.6.1
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
ENV LANGUAGE=C.UTF-8

# Install poetry
RUN python -m pip install "poetry==$POETRY_VERSION"
RUN poetry config virtualenvs.create false

# Install dependencies needed by perch
RUN apt-get -y update && apt-get -y install libsndfile1 ffmpeg
 
# Project set up
WORKDIR /root

# Copy helping scripts for dev container
COPY pyproject.toml poetry.lock /root/

# Install perch environment
RUN poetry install --with jaxtrain