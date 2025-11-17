# npx @tailwindcss/cli -i ./src/frontend_canister/src/index.scss -o ./src/frontend_canister/src/output.css --watch

clear
# mops test

dfx stop
rm -rf .dfx
dfx start --clean --background

echo "$(dfx identity use default)"
export DEFAULT_ACCOUNT_ID=$(dfx ledger account-id)
echo "DEFAULT_ACCOUNT_ID: " $DEFAULT_ACCOUNT_ID
export DEFAULT_PRINCIPAL=$(dfx identity get-principal)

# dfx extension install nns
# dfx nns install --ledger-accounts $DEFAULT_ACCOUNT_ID

# dfx ledger fabricate-cycles --canister <canister_name> --t 8


