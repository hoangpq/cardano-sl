module Explorer.I18n.JP where

import Explorer.I18n.Types (Translation)

translation :: Translation
translation =
    { common:
        { cBack: "戻る"
        , cApi: "API"
        , cADA: "ADA"
        , cBCshort: "BC"
        , cBCong: "Bitcoin"
        , cTransaction: "トランザクション"
        , cTransactions: "トランザクション"
        , cTransactionFeed: "トランザクションフィード"
        , cAddress: "アドレス"
        , cVersion: "バージョン"
        , cCalculator: "計算機"
        , cNetwork: "ネットワーク"
        , cSummary: "概要"
        , cBlock: "スロット"
        , cHash: "ハッシュ値"
        , cHashes: "ハッシュ値"
        , cEpoch: "エポック"
        , cEpochs: "エポック"
        , cSlot: "スロット"
        , cSlots: "スロット"
        , cAge: "経過時間"
        , cTotalSent: "総送信額"
        , cRelayedBy: "スロットリーダー"
        , cSizeKB: "サイズ (kB)"
        , cExpand: "開く"
        , cCollapse: "閉じる"
        , cNoData: "データなし"
        , cTitle: "Cardano Blockchain Explorer"
        , cCopyright: "Cardano Blockchain Explorer @2017"
        , cUnknown: "不明"
        , cTotalOutput: "合計アウトプット"
        , cOf: "of"
        , cNotAvailable: "データがありません"
        , cLoading: "ロード中..."
        , cBack2Dashboard: "ダッシュボードに戻る"
        , cDays: "日"
        , cHours: "時間"
        , cMinutes: "分"
        , cSeconds: "秒"
        , cDateFormat: "YYYY/MM/DD HH:mm:ss"
        }
    , navigation:
        { navHome: "ホーム"
        , navBlockchain: "ブロックチェーン"
        , navMarket: "マーケット"
        , navCharts: "チャート"
        , navTools: "ツール"
        }
    , hero:
        {  hrSubtitle: "カルダノネットワーク上のアドレス、トランザクション、エポック、スロットを検索"
        , hrSearch: "スロット、エポック、トランザクションを検索"
        , hrTime: "タイム"
        }
    , dashboard:
        { dbTitle: "ダッシュボード"
        , dbLastBlocks: "最近のスロット"
        , dbLastBlocksDescription: "{0}に{1}のトランザクションが生成されました"
        , dbPriceAverage: "平均価格"
        , dbPriceForOne: "1{1}の価格: {0}"
        , dbPriceSince: "昨日からの価格: {0}"
        , dbTotalSupply: "総供給額"
        , dbTotalAmountOf: "システム内総額: {0}"
        , dbTotalAmountOfTransactions: "開設以来システム内で検出された総トランザクション数"
        , dbExploreBlocks: "スロットを検索する"
        , dbExploreTransactions: "トランザクションを検索する"
        , dbBlockchainOffer: "ブロックチェーンエクスプローラーから提供されるもの"
        , dbBlockSearch: "スロット検索"
        , dbBlockSearchDescription: "スロットはトランザクションを格納しているボックスです。"
        , dbAddressSearch: "アドレス検索"
        , dbAddressSearchDescription: "アドレスを検索します"
        , dbTransactionSearch: "トランザクション検索"
        , dbTransactionSearchDescription: "トランザクションはAからBへコインを送金することです。"
        , dbApiDescription: "我々の安全なAPIは、さまざまな言語とSDKで利用可能です。"
        , dbGetAddress: "アドレス取得"
        , dbResponse: "レスポンス"
        , dbCurl: "Curl"
        , dbNode: "Node"
        , dbJQuery: "jQuery"
        , dbGetApiKey: "APIキー取得"
        , dbMoreExamples: "その他の例を見る"
        , dbAboutBlockchain: "ブロックチェーンについて"
        , dbAboutBlockchainDescription: "ブロックチェーンAPIはより容易に暗号通貨のアプリケーション及び機能を実装することができます。我々は開発者が高速、スケーラブル、かつ安全なサービスを構築できるようなプラットフォームを提供することに注力しています。<br/><br/>このAPIはベータ版では無料で無制限です。APIはまだ初期段階であり、今後より多くのエンドポイント、機能の導入を予定しています。開発チームはユーザーが求めているAPIを構築したいので、リクエスト、改善点などがあればご連絡お願いします。"
        }
    , address:
        { addScan: "QRコードをスキャンしてアドレスをクリップボードにコピーする"
        , addQrCode: "QRコード"
        , addFinalBalance: "最終残高"
        , addNotFound: "アドレスは存在しません"
        }
    , tx:
        { txTime: "入金時刻"
        , txIncluded: "含まれているスロット"
        , txRelayed: "中継IP"
        , txEmpty: "トランザクションなし"
        , txFees: "トランザクション料"
        , txNotFound: "トランザクションは見つかりません"
        }
    , block:
        { blFees: "手数料"
        , blEstVolume: "推定容量"
        , blPrevBlock: "前のスロット"
        , blNextBlock: "次のスロット"
        , blRoot: "ハッシュ根"
        , blNotFound: "エポック / スロットは存在していません。"
        }
    , footer:
        { fooResources: "Resources"
        , fooFollow: "Follow us"
        , fooLinks: "Links"
        , fooIohkSupportP: "IOHK supported project"
        , fooDocumentation: "Documentation"
        , fooGithub: "Github"
        , fooLinkedin: "Linkedin"
        , fooTwitter: "Twitter"
        , fooDaedalusWallet: "Daedalus Wallet"
        , fooWhyCardano: "Why Cardano"
        , fooCardanoRoadmap: "Cardano Roadmap"
        , fooCardanoADAFaucet: "Cardano ADA Faucet"
        , fooCardanoSLDocumentation: "Cardano SL Documentation"
        }
    , notfound:
        { nfTitle: "404"
        , nfDescription: "ページが見つかりません"
        }
    }
