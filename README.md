# 🏦 Unifyfi - Credit Union Smart Wallet

> 💰 Automate dividend payouts to contributors with blockchain transparency

## 🌟 Overview

Unifyfi is a decentralized credit union smart contract built on Stacks blockchain that automatically manages member contributions and distributes dividends based on participation. Members contribute STX tokens and earn dividends proportional to their contribution share over time.

## ✨ Key Features

- 🤝 **Member Management**: Join the credit union with initial contributions
- 💎 **Contribution Tracking**: Track all member contributions with block-level history  
- 📈 **Automatic Dividends**: Calculate dividends based on contribution share and time
- 🔄 **Auto-Payout System**: Batch dividend distribution to multiple members
- 👑 **Admin Controls**: Owner can manage rates and member status
- 🛡️ **Emergency Functions**: Safety mechanisms for fund protection

## 🚀 Getting Started

### Prerequisites
- Clarinet CLI installed
- Stacks wallet with STX tokens

### Installation

```bash
git clone <your-repo>
cd unifyfi
clarinet check
```

## 📋 Usage Instructions

### For Members

#### 1️⃣ Join Credit Union
```clarity
(contract-call? .Unifyfi join-credit-union u1000000) ;; Join with 1 STX
```

#### 2️⃣ Make Additional Contributions  
```clarity
(contract-call? .Unifyfi contribute u500000) ;; Contribute 0.5 STX
```

#### 3️⃣ Claim Your Dividends
```clarity
(contract-call? .Unifyfi claim-dividends)
```

#### 4️⃣ Check Your Info
```clarity
(contract-call? .Unifyfi get-member-info 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

### For Administrators

#### 🎛️ Set Dividend Rate
```clarity
(contract-call? .Unifyfi set-dividend-rate u750) ;; Set to 0.075%
```

#### 💸 Auto-Payout to Multiple Members
```clarity
(contract-call? .Unifyfi auto-payout-dividends (list 'ST1... 'ST2... 'ST3...))
```

## 📊 Read-Only Functions

| Function | Description | Example |
|----------|-------------|---------|
| `get-member-info` | 👤 Get member details | `(contract-call? .Unifyfi get-member-info 'ST1...)` |
| `get-total-pool` | 💰 Total STX in pool | `(contract-call? .Unifyfi get-total-pool)` |
| `get-total-members` | 👥 Active member count | `(contract-call? .Unifyfi get-total-members)` |
| `get-contract-balance` | 🏦 Contract STX balance | `(contract-call? .Unifyfi get-contract-balance)` |
| `is-member` | ✅ Check membership status | `(contract-call? .Unifyfi is-member 'ST1...)` |

## 🔧 Configuration

### Dividend Rate
- Default: 500 (0.05% per block)
- Range: 0-2000 (0-0.2% per block)
- Only contract owner can modify

### Member Requirements
- Minimum contribution: 1 microSTX
- Must be active member to earn dividends
- Dividends calculated from last claim block

## 🛡️ Security Features

- ✅ Owner-only administrative functions
- ✅ Member validation for all operations  
- ✅ Balance checks before transfers
- ✅ Emergency withdrawal capability
- ✅ Member deactivation controls

## 📈 Dividend Calculation

Dividends = (Member Share × Dividend Rate × Blocks Since Last Claim) / 1,000,000

Where:
- **Member Share** = (Member Contribution / Total Pool) × 10,000
- **Dividend Rate** = Configurable rate (default 500)
- **Blocks Since Last Claim** = Current block - Last claim block

## 🚨 Error Codes

| Code | Description |
|------|-------------|
| u100 | Not authorized |
| u101 | Insufficient balance |
| u102 | Member not found |
| u103 | Already a member |
| u104 | Invalid amount |
| u105 | No dividends available |
| u106 | Transfer failed |

## 🤝 Contributing

1. Fork the repository
2. Create feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m 'Add amazing feature'`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Open Pull Request

## 📄 License

This project is licensed under the MIT License.



