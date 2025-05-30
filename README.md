# M Type Checker

## 의존성 설치

```sh
λ opam install . --deps-only
```

이번 과제를 컴파일하기 위해서는 외부 라이브러리 `ocaml-compiler-libs`가 필요합니다.
위 명령을 실행하여 필요한 의존성을 설치하여 주시기 바랍니다.

## 컴파일 및 실행 방법

```sh
λ dune build --release
λ ./run examples/test1.m
```

실행 시 파일명을 명시하지 않을 경우, 표준 입력으로부터 실행 코드를 읽어들입니다.
표준 입력으로 프로그램을 입력하신 후, 첫 번째 칸(column)에서 유닉스 머신에서는 Ctrl-D, 윈도우 환경에서는 Ctrl-Z를 누르시면 프로그램이 실행됩니다.

## 파스 트리 출력하기

아래와 같이 하면 파싱된 구문 구조를 출력한 다음 실행합니다.

```sh
λ ./run -pp examples/test1.m
```

# M Language Server

`poly_checker.ml` 파일을 완성하셨다면, M 언어 서버를 빌드하여 자신의 타입 검사기가 정상적으로 작동하는지 시각적으로 확인할 수 있습니다. Visual Studio Code에서 M 언어 서버를 빌드하는 방법은 아래와 같습니다.

## Step 1. OCaml 파일 빌드하기

```sh
λ dune build —-release
```

## Step 2. TypeScript 파일 빌드하기

```sh
λ npm install && npm run compile
```

## Step 3. VSIX 빌드하기

```sh
λ vsce package
```

위 코드를 실행하면 언어 서버의 패키지 파일인 `mlsp-0.0.1.vsix` 파일이 생성됩니다.

## Step 4. Extension 추가하기

Extensions 탭의 상단 오른쪽에 `···` 메뉴가 있습니다.
이 메뉴에서 ‘Install from VSIX…’를 클릭하면, 조금 전 생성한 vsix 파일을 가져올 수 있습니다.
파일을 가져온 다음 M 언어로 작성된 파일을 확인하면 언어 서버 프로토콜이 정상적으로 작동하는 모습을 볼 수 있습니다.
