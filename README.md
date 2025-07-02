# LED-A
Software for calculating and visualizing linguistic distances among dialect varieties using the Relative Identity Distance, Levenshtein distance, Dynamic Time Warping, and the POS-tag <i>n</i>-gram frequency method.

Follow the instructions below to build the Docker image and launch the container.

### 1. Clone the Repo

```
git clone https://github.com/fryske-akademy/LED-A.git
cd LED-A
```

### 2. Build the Docker Image

```
docker build -t led-a .
```

### 3. Run the Container

```
docker run -p 3838:3838 led-a
```

### 4. View in Browser

Open:
http://localhost:3838
