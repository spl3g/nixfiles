{
  lib,
  config,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.kube;
in
{
  options.kube = {
    enable = mkEnableOption "Whether to enable the k3s+flux config";
    sopsKeyFile = mkOption {
      description = "Path to a file with a key for sops secrets";
      type = types.path;
    };
    tokenFile = mkOption {
      description = "Path to a file with the token for agents";
      type = types.path;
    };
    nodeIP = mkOption {
      type = types.str;
    };
    nodeLabel = mkOption {
      type = types.listOf types.str;
    };
    repoUrl = mkOption {
      description = "A repo url for flux to use";
      type = types.str;
      default = "https://codeberg.org/spl3g/kubefiles";
    };
  };

  config = mkIf cfg.enable {
    services.k3s = {
      enable = true;
      role = "server";
      agentTokenFile = config.age.secrets.kube-token.path;
      clusterInit = true;
      nodeIP = cfg.nodeIP;
      nodeLabel = cfg.nodeLabel;
      extraFlags = [
        "--disable traefik"
        "--disable servicelb"
        "--flannel-iface wg0"
      ];
      manifests = {
        flux-namespace = {
          content = {
            apiVersion = "v1";
            kind = "Namespace";
            metadata = {
              name = "flux-system";
              labels = {
                "pod-security.kubernetes.io/enforce" = "privileged";
              };
            };
          };
        };

        flux-operator = {
          content = {
            apiVersion = "helm.cattle.io/v1";
            kind = "HelmChart";
            metadata = {
              name = "flux-operator";
              namespace = "kube-system";
            };
            spec = {
              targetNamespace = "flux-system";
              createNamespace = false;
              chart = "oci://ghcr.io/controlplaneio-fluxcd/charts/flux-operator";
            };
          };
        };

        flux-instance = {
          content = {
            apiVersion = "fluxcd.controlplane.io/v1";
            kind = "FluxInstance";
            metadata = {
              name = "flux";
              namespace = "flux-system";
              annotations = {
                "fluxcd.controlplane.io/reconcileEvery" = "1h";
                "fluxcd.controlplane.io/reconcileTimeout" = "10m";
              };
            };
            spec = {
              distribution = {
                version = "2.x";
                registry = "ghcr.io/fluxcd";
                artifact = "oci://ghcr.io/controlplaneio-fluxcd/flux-operator-manifests";
              };
              components = [
                "source-controller"
                "source-watcher"
                "kustomize-controller"
                "helm-controller"
                "notification-controller"
                "image-reflector-controller"
                "image-automation-controller"
              ];
              cluster = {
                type = "kubernetes";
                multitenant = false;
                networkPolicy = true;
                domain = "cluster.local";
              };
              sync = {
                kind = "GitRepository";
                url = cfg.repoUrl;
                ref = "refs/heads/main";
                path = "clusters/ltrr";
                interval = "1m";
              };
            };
          };
        };
      };
    };

    systemd.services.flux-sops-secret = {
      description = "Create Flux SOPS age key secret";
      after = ["k3s.service" "agenix.service"];
      requires = ["k3s.service"];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = ''
        export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
        KUBECTL="${pkgs.kubectl}/bin/kubectl"

        for i in $(seq 1 60); do
          if $KUBECTL get --raw /healthz >/dev/null 2>&1; then
            break
          fi
          echo "Waiting for k3s API (attempt $i)..."
          sleep 5
        done

        $KUBECTL create namespace flux-system --dry-run=client -o yaml | \
          $KUBECTL apply --validate=false -f -

        $KUBECTL create secret generic sops-age \
          --namespace=flux-system \
          --from-file=age.agekey=${cfg.sopsKeyFile} \
          --dry-run=client -o yaml | \
          $KUBECTL apply --validate=false -f -
      '';
      wantedBy = ["multi-user.target"];
    };
  };
}
