#!/bin/bash

die() {
	echo >&2 "$@"
	exit 1
}

[ "$#" -eq 2 ] || die "2 arguments required, $# provided"
echo $1 | grep -E -q '^[0-9]+$' || die "Number required, $1 provided"
echo $2 | grep -E -q '^[0-9]+$' || die "Number required, $1 provided"

Year=$1
Day=$2

pushd "$(dirname "$0")" > /dev/null


[ ! "$AOC_SESSION" = "" ] || die "AOC_SESSION env variable not set"
p=$(printf "Day%02d" $Day)

# download the file
curl -HGET https://adventofcode.com/$Year/day/$Day/input -H "Cookie: session=$AOC_SESSION" > $Year/$p/input.txt

name="$Year/$p/$p.fsproj"

if ! grep -q "input.txt" $name; then 
	xmlstarlet edit -L \
		  --append '/Project/ItemGroup/Compile' --type elem -n Content \
		  --insert '/Project/ItemGroup/Content[not(@Include)]' -t attr -n "Include" -v "input.txt" \
		  --subnode '/Project/ItemGroup/Content[@Include="input.txt"]' -t elem -n "CopyToOutputDirectory" -v "PreserveNewest" \
		  $name
fi

if ! grep -q "sample.txt" $name; then 
	xmlstarlet edit -L \
		  --append '/Project/ItemGroup/Content[@Include="input.txt"]' --type elem -n Content \
		  --insert '/Project/ItemGroup/Content[not(@Include)]' -t attr -n "Include" -v "sample.txt" \
		  --subnode '/Project/ItemGroup/Content[@Include="sample.txt"]' -t elem -n "CopyToOutputDirectory" -v "PreserveNewest" \
		  $name
fi

cat $name
cat $Year/$p/input.txt

touch $Year/$p/sample.txt

popd > /dev/null
