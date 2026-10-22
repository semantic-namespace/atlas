import { SHOP_ENDPOINTS } from '../api/endpoints';

export class OrderService {
  constructor(private readonly client: any) {}

  async order(userId: string, orderId: string) {
    return this.client.get({ url: SHOP_ENDPOINTS.order(userId, orderId) });
  }

  async place(userId: string) {
    const url = SHOP_ENDPOINTS.placeOrder(userId);
    return this.client.get({ url });
  }

  async list(userId: string) {
    const url = SHOP_ENDPOINTS.orders(userId);
    return this.client.post({ url, data: {} });
  }
}
