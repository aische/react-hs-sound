
echo "module CommitHash (commitHash) where" > src/CommitHash.hs

H=$(git rev-parse HEAD)

echo "commitHash = \"$H\"" >> src/CommitHash.hs

