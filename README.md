SNU 4190.310 Programming Languages

# M Type Checker

## 컴파일 및 실행 방법

```sh
make
./run examples/test1.m
```

실행 시 파일명을 명시하지 않을 경우, 표준 입력으로부터 실행 코드를 읽어들입니다.
표준 입력으로 프로그램을 입력하신 후, 첫 번째 칸(column)에서 유닉스 머신에서는 Ctrl-D, 윈도우 환경에서는 Ctrl-Z를 누르시면 프로그램이 실행됩니다.
정상적으로 실행되는 경우 표준 출력 없이 종료되고, 실행 과정에서 문제가 있었다면 오류가 출력됩니다.

## 숙제 제출 관련

`poly_checker.ml` 파일에서 `infer` 함수를 구현하고 그 파일만 제출합니다.
이때, `infer` 함수의 타입은 변형하셔도 채점에는 영향이 없으나 M 언어 서버와 호환되지 않을 수 있으니 주의해 주세요.

## 참고 사항

`syntax.ml`에 M의 문법 및 타입, `interp.ml`에 실행기의 정의가 있으니 한번 살펴보는 것을 추천드립니다.

# M Language Server

poly_checker 파일을 완성하셨다면, M 언어 서버를 빌드하여 자신의 타입 검사기가 정상적으로 작동하는지 시각적으로 확인할 수 있습니다. Visual Studio Code에서 M 언어 서버를 빌드하는 방법은 아래와 같습니다.

## Step 1. OCaml 파일 빌드하기

```sh
dune build —release
```

설치되지 않은 라이브러리가 있다면 다음 코드를 실행하여 필요한 라이브러리를 설치해야 합니다.

```sh
opam install . —deps-only
```

## Step 2. TypeScript 파일 빌드하기

```sh
npm install && npm run compile
```

## Step 3. VSIX 빌드하기

```sh
vsce package
```

위 코드를 실행하면 언어 서버의 패키지 파일인 `mlsp-0.0.1.vsix` 파일이 생성됩니다.

## Step 4. Extension 추가하기

Extensions 탭의 상단 오른쪽에 `···` 메뉴가 있습니다.
이 메뉴에서 ‘Install from VSIX…’를 클릭하면, 조금 전 생성한 vsix 파일을 가져올 수 있습니다.
파일을 가져온 다음 M 언어로 작성된 파일을 확인하면 언어 서버 프로토콜이 정상적으로 작동하는 모습을 볼 수 있습니다.