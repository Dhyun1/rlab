# 326.212 Final Project

### Due Dec 11, 2020 @ 11:59pm

[2020년 전산통계 및 실험 기말 프로젝트](https://won-j.github.io/326_212-2020fall/project/project_problem.html)입니다.  
프로젝트를 수행하기 전에 아래 내용을 필히 숙지하길 바랍니다.  
[Project FAQ](https://github.com/snu-stat/sc20-commonplace/issues/35) 또한 참고해 주세요.

## 뼈대코드

프로젝트를 위해 뼈대코드를 제공합니다. 아래의 설명을 참고하여 `.gitignore`, `part1.Rmd`, `part2.Rmd`, `part3_q1.Rmd`, `part3_q2.Rmd` 파일을 수정해 주세요.

- `data/`: 프로젝트에 사용할 자료가 들어갈 폴더입니다.
    + 로컬 상에서 `.csv` 파일들을 이 폴더 안에 다운로드한 다음 프로젝트를 진행하시길 바랍니다.
- `.gitignore`: 이 파일의 하단에 아래의 내용을 추가해 주세요.
```
# Data files
*.csv
*.zip
*.dbf
*.prj
*.shp
*.shx
```
- `part1.Rmd`: Part1 문제를 여기에 푼 뒤, html을 knit해 주세요.
- `part2.Rmd`: Part2 문제를 여기에 푼 뒤, html을 knit해 주세요.
- `part3_q1.Rmd`: Part3-Q1 문제를 여기에 푼 뒤, html을 knit해 주세요.
- `part3_q2.Rmd`: Part3-Q2 문제를 여기에 푼 뒤, html을 knit해 주세요.

## 주의사항

- **`.zip`, `.csv` 와 같은 데이터 파일들을 절대 commit하지 마세요.**
- **`.gitignore` 파일을 삭제하지 마세요.**
- **최종 commit 시 처음 주어진 파일과 이름이 다른 파일이 GitHub 상에 올라가 있으면 안됩니다. `master` 브랜치의 모든 파일의 이름이 `project` 브랜치와 같은 이름으로 되어 있는지를 꼭 확인해주세요.**