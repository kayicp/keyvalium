# npx @tailwindcss/cli -i ./src/frontend/src/index.scss -o ./src/frontend/src/output.css --watch

clear
# mops test

dfx stop
rm -rf .dfx
dfx start --clean --background

echo "$(dfx identity use default)"
export DEFAULT_ACCOUNT_ID=$(dfx ledger account-id)
echo "DEFAULT_ACCOUNT_ID: " $DEFAULT_ACCOUNT_ID
export DEFAULT_PRINCIPAL=$(dfx identity get-principal)

export INTERNET_ID="rdmx6-jaaaa-aaaaa-aaadq-cai"
export ICP_ID="ryjl3-tyaaa-aaaaa-aaaba-cai"
export TCYCLES_ID="um5iw-rqaaa-aaaaq-qaaba-cai"

dfx deploy icp_token --no-wallet --specified-id $ICP_ID --argument "(
  variant {
    Init = record {
      token = record {
        fee = 10_000 : nat;
        decimals = 8 : nat;
        name = \"Internet Computer\";
        minter = principal \"$DEFAULT_PRINCIPAL\";
        permitted_drift_secs = 60 : nat;
        tx_window_secs = 3_600 : nat;
        max_supply = 100_000_000_000_000_000 : nat;
        max_memo_size = 32 : nat;
        min_memo_size = 1 : nat;
        symbol = \"ICP\";
        max_approval_expiry_secs = 2_592_000 : nat;
      };
			vault = null;
      archive = record {
        min_creation_tcycles = 4 : nat;
        max_update_batch = 10 : nat;
      };
    }
  },
)"

dfx deploy tcycles_token --no-wallet --specified-id $TCYCLES_ID --argument "(
  variant {
    Init = record {
      token = record {
        fee = 100_000_000 : nat;
        decimals = 12 : nat;
        name = \"Trillion Cycles\";
        minter = principal \"$DEFAULT_PRINCIPAL\";
        permitted_drift_secs = 60 : nat;
        tx_window_secs = 3_600 : nat;
        max_supply = 100_000_000_000_000_000 : nat;
        max_memo_size = 32 : nat;
        min_memo_size = 1 : nat;
        symbol = \"TCYCLES\";
        max_approval_expiry_secs = 2_592_000 : nat;
      };
			vault = null;
      archive = record {
        min_creation_tcycles = 4 : nat;
        max_update_batch = 10 : nat;
      };
    }
  },
)"

dfx deploy kv_backend --no-wallet --argument "(
  variant {
    Init = record {
      main = record {
				secs = record {
					tx_window = 3_600 : nat;
					permitted_drift = 60 : nat;
					min_duration = 3600 : nat;
				};
				max = record {
					take_value = 100 : nat;
					update_batch_size = 100 : nat;
					query_batch_size = 100 : nat;
				};
				fee_collector = principal \"$DEFAULT_PRINCIPAL\";
				withdrawal_fee_multiplier = 3 : nat;
				premium_percent = 200 : nat;
      };
      archive = record {
        min_creation_tcycles = 4 : nat;
        max_update_batch = 10 : nat;
      };
    }
  },
)"

dfx deploy frontend_canister --no-wallet

# dfx ledger fabricate-cycles --canister <canister_name> --t 8


