# Use an official Python base image
FROM python:3.12-slim

# Set working directory
WORKDIR /app

# Copy requirement file first (better caching)
COPY requirements.txt .

# Install dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Copy all your files
COPY . .

# Expose default Streamlit port (we’ll override later)
EXPOSE 8501

# Command will be set in docker-compose or when running container